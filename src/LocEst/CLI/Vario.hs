{-# LANGUAGE BangPatterns        #-}

module LocEst.CLI.Vario where

import LocEst.Parsers
import LocEst.Types
import LocEst.Distance

import           System.IO       (hPutStrLn, stderr)
import Data.List (tails, transpose)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Conduit.List as ConL
import Conduit ((.|))
import qualified Data.Conduit as Con

data VarioOptions = VarioOptions {
    _voInObservationFile :: FilePath,
    _voVariogramOutFile  :: Maybe FilePath
}

runVario :: VarioOptions -> IO ()
runVario (VarioOptions inObsFile outVariogramFile) = do
    -- read observations
    hPutStrLn stderr "Reading observations"
    !observationsUnindexed <- readObservations inObsFile
    let observations = zipWith setIndex observationsUnindexed [0..]
    -- calculate pairwise distances
    hPutStrLn stderr "Calculating pairwise distances for independent variables"
    let distsPerIndepVar = calcIndepVarPairwiseDistances observations
    hPutStrLn stderr "Calculating pairwise distances for dependent variables"
    let distsPerDepVar   = calcDepVarPairwiseDistances observations
    -- determine bins
    hPutStrLn stderr "Determine bins for independent variables"
    --error $ show distsPerIndepVar
    let perIndepVar  = map binIndepVar distsPerIndepVar
    -- iterate over all permutations of indepVars and depVars to calculate empirical variograms
    hPutStrLn stderr "Calculate empirical variograms"
    let empiricalVariograms = concat $
            for perIndepVar $ \(indepVarName, SUDistMatrix indepDists, steps) ->
                let indicesPerBin = map (findIndicesForBin indepDists) steps
                in for distsPerDepVar $ \(depVarName, SUDistMatrix depDists) ->
                        let dots = for indicesPerBin $ \(mid, indicesForOneBin) ->
                                let depDistsPerBin = VU.map (depDists VU.!) indicesForOneBin
                                    semivariance = calcMatheron depDistsPerBin
                                in (mid, semivariance)
                    in EmpiricalVariogramOneVarCombination indepVarName depVarName (EmpiricalVariogram dots)
    writeVariograms empiricalVariograms outVariogramFile

    -- fit theoretical variograms

    -- huhu
    hPutStrLn stderr "wip"

writeVariograms :: [EmpiricalVariogramOneVarCombination] -> Maybe FilePath -> IO ()
writeVariograms _ Nothing        = return ()
writeVariograms vars (Just path) = Con.runConduitRes $ ConL.sourceList (concatMap varToLong vars) .| sinkNamedCSV path

varToLong :: EmpiricalVariogramOneVarCombination -> [EmpiricalVariogramSingleBin]
varToLong (EmpiricalVariogramOneVarCombination i d (EmpiricalVariogram xs)) =
    map (\(iv, dv) -> EmpiricalVariogramSingleBin i d iv dv) xs

-- half mean squared distance within one bin
calcMatheron :: VU.Vector Double -> Double
calcMatheron dists = (1 / (2 * n)) * VU.foldl' (\acc d -> acc + (d ** 2)) 0 dists
    where
        n = fromIntegral $ VU.length dists

for :: (Functor f) => f a -> (a -> b) -> f b
for = flip fmap

findIndicesForBin :: VU.Vector Double -> (Double, Double, Double) -> (Double, VU.Vector Int)
findIndicesForBin vec (lo, mid, hi) = (mid, VU.findIndices (\x -> lo <= x && x < hi) vec)

binIndepVar :: (IndepVarName, SUDistMatrix) -> (IndepVarName, SUDistMatrix, [(Double, Double, Double)])
binIndepVar (indepVarName, dist@(SUDistMatrix distVec)) =
    let minValue = VU.minimum distVec
        maxValue = VU.maximum distVec
        endVario = minValue + (maxValue - minValue)/3
        stepWidth = (endVario - minValue)/1000
        stepsSingle = [minValue,minValue+stepWidth..endVario]
        steps = zipWith (\lo hi -> (lo,lo+(hi-lo)/2,hi)) (init stepsSingle) (tail stepsSingle)
    in (indepVarName, dist, steps)

calcIndepVarPairwiseDistances :: [Observation] -> [(IndepVarName, SUDistMatrix)]
calcIndepVarPairwiseDistances obs = reshape [dist x y | y <- obs, (x:_) <- tails obs]
    where
        dist :: Observation -> Observation -> [(IndepVarName, Double)]
        dist
            (Observation _ _ (HyperPos (IndepSpatTempPos p1) _))
            (Observation _ _ (HyperPos (IndepSpatTempPos p2) _)) =
            [("space", spatialDistSpatTempPos p1 p2), ("time", temporalDistSpatTempPos p1 p2)]
        dist
            (Observation _ _ (HyperPos (IndepArbitraryDimPos p1) _))
            (Observation _ _ (HyperPos (IndepArbitraryDimPos p2) _)) =
            -- this assumes that p1 and p2 have the same order of indep variables
            zip (getKeys p1) (allDistances (getValues p1) (getValues p2))
        dist _ _ = error "Impossible state in indep distance calculation"

calcDepVarPairwiseDistances :: [Observation] -> [(DepVarName, SUDistMatrix)]
calcDepVarPairwiseDistances obs = reshape [dist x y | y <- obs, (x:_) <- tails obs]
    where
        dist :: Observation -> Observation -> [(DepVarName, Double)]
        dist
            (Observation _ _ (HyperPos _ p1))
            (Observation _ _ (HyperPos _ p2)) =
            -- this assumes that p1 and p2 have the same order of dep variables
            zip (getKeys p1) (allDistances (getValues p1) (getValues p2))

reshape :: [[(String, Double)]] -> [(String, SUDistMatrix)]
reshape xss = map reshapeOne $ transpose xss
    where
        reshapeOne :: [(String, Double)] -> (String, SUDistMatrix)
        reshapeOne xs =
            let name = fst $ head xs
                matrix = SUDistMatrix $ VU.fromList $ map snd xs
            in (name, matrix)

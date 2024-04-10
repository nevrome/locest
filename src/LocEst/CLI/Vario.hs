{-# LANGUAGE BangPatterns        #-}

module LocEst.CLI.Vario where

import LocEst.Parsers
import LocEst.Types
import LocEst.Distance

import           System.IO       (hPutStrLn, stderr)
import Data.List (tails, transpose)
import qualified Data.Vector.Unboxed as VU

data VarioOptions = VarioOptions {
    _voInObservationFile :: FilePath,
    _voVariogramOutFile  :: Maybe FilePath
}

runVario :: VarioOptions -> IO ()
runVario (VarioOptions inObsFile _) = do
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
                                let depVarVals = VU.map (depDists VU.!) indicesForOneBin
                                    semivariance = calcMatheron depVarVals
                                in (mid, semivariance)
                    in (indepVarName, depVarName, dots)
    print empiricalVariograms
    -- fit theoretical variograms

    -- huhu
    hPutStrLn stderr "wip"

for :: (Functor f) => f a -> (a -> b) -> f b
for = flip fmap

findIndicesForBin :: VU.Vector Double -> (Double, Double, Double) -> (Double, VU.Vector Int)
findIndicesForBin vec (lo, mid, hi) = (mid, VU.findIndices (\x -> lo <= x && x < hi) vec)

binIndepVar :: (IndepVarName, SUDistMatrix) -> (IndepVarName, SUDistMatrix, [(Double, Double, Double)])
binIndepVar (indepVarName, dist@(SUDistMatrix distVec)) =
    let minValue = VU.minimum distVec
        maxValue = VU.maximum distVec
        stepWidth = (maxValue - minValue)/20
        stepsSingle = [minValue,minValue+stepWidth..maxValue]
        steps = zipWith (\lo hi -> (lo,lo+(hi-lo)/2,hi)) (init stepsSingle) (tail stepsSingle)
    in (indepVarName, dist, steps)

calcMatheron :: VU.Vector Double -> Double
calcMatheron xs = (1 / (2 * n)) * VU.foldl' (\acc x -> acc + ((x - mean) ** 2)) 0 xs
    where
        n = fromIntegral $ VU.length xs
        mean = VU.sum xs / n

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
reshape xs = map reshapeOne $ transpose xs
    where
        reshapeOne :: [(String, Double)] -> (String, SUDistMatrix)
        reshapeOne xs =
            let name = fst $ head xs
                matrix = SUDistMatrix $ VU.fromList $ map snd xs
            in (name, matrix)

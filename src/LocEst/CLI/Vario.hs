{-# LANGUAGE BangPatterns        #-}
-- {-# LANGUAGE Strict        #-}

module LocEst.CLI.Vario where

import LocEst.Parsers
import LocEst.Types
import LocEst.Distance

import           System.IO       (hPutStrLn, stderr)
import Data.List (tails)
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Conduit.List as ConL
import Conduit ((.|))
import qualified Data.Conduit as Con
import Control.Monad (zipWithM_, replicateM)
import Data.Function (on)

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
    distsPerIndepVar <- calcIndepVarPairwiseDistances observations
    hPutStrLn stderr "Calculating pairwise distances for dependent variables"
    distsPerDepVar   <- calcDepVarPairwiseDistances observations
    -- determine bins
    hPutStrLn stderr "Determining bins for independent variables"
    --error $ show distsPerIndepVar
    let perIndepVar  = map binIndepVar distsPerIndepVar
    -- iterate over all permutations of indepVars and depVars to calculate empirical variograms
    hPutStrLn stderr "Calculating empirical variograms"
    empiricalVariograms <- fmap concat $
        forM perIndepVar $ \(indepVarName, SUDistMatrix indepDists, bins) -> do
            hPutStrLn stderr ("Working on " ++ indepVarName)
            -- remove dists that are not binnable
            sortedIndepDists <- sortWithIndices indepDists
            let (_,_,lastHi) = last bins
                (binnableIndepDists,_) = VU.span (\(i,v) -> v <= lastHi) sortedIndepDists
            let indicesPerBin = map (findIndicesForBin binnableIndepDists) bins
            forM distsPerDepVar $ \(depVarName, SUDistMatrix depDists) -> do
                hPutStrLn stderr ("-> " ++ depVarName)
                let !semivariancesPerBin = for indicesPerBin $ \(mid, indicesForOneBin) ->
                        let depDistsPerBin = VU.map (depDists VU.!) indicesForOneBin
                            semivariance = calcMatheron depDistsPerBin
                        in (mid, semivariance)
                return $ EmpiricalVariogramOneVarCombination indepVarName depVarName (EmpiricalVariogram semivariancesPerBin)
    -- write variograms to the file system
    writeVariograms empiricalVariograms outVariogramFile

findIndicesForBin :: VU.Vector (Int, Double) -> (Double, Double, Double) -> (Double, VU.Vector Int)
findIndicesForBin vec (lo, mid, hi) = (mid, VU.map fst $ VU.filter (\(_,v) -> lo <= v && v < hi) vec)

sortWithIndices :: VU.Vector Double -> IO (VU.Vector (Int, Double))
sortWithIndices v = do
  let v' = VU.indexed v  -- Pair each element with its index
  mv <- VU.thaw v'       -- Create a mutable copy
  VA.sortBy (compare `on` snd) mv  -- Sort it in-place
  VU.unsafeFreeze mv     -- Convert back to a pure vector

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

forM :: Monad m => [a] -> (a -> m b) -> m [b]
forM = flip mapM

for :: [a] -> (a -> b) -> [b]
for = flip map

binIndepVar :: (IndepVarName, SUDistMatrix) -> (IndepVarName, SUDistMatrix, [(Double, Double, Double)])
binIndepVar (indepVarName, dist@(SUDistMatrix distVec)) =
    let minValue = VU.minimum distVec
        maxValue = VU.maximum distVec
        endVario = minValue + (maxValue - minValue)/3
        stepWidth = (endVario - minValue)/1000
        stepsSingle = [minValue,minValue+stepWidth..endVario]
        steps = zipWith (\lo hi -> (lo,lo+(hi-lo)/2,hi)) (init stepsSingle) (tail stepsSingle)
    in (indepVarName, dist, steps)

calcIndepVarPairwiseDistances :: [Observation] -> IO [(IndepVarName, SUDistMatrix)]
calcIndepVarPairwiseDistances obs = do
    let indexPairs = zip [0..] [ (x,y) | y <- obs, (x:_) <- tails obs]
        nrPairs = length indexPairs
        (Observation _ _ (HyperPos indepPos _)) = head obs
    case indepPos of
        IndepSpatTempPos _ -> do
            spaceVec <- VUM.new nrPairs
            timeVec  <- VUM.new nrPairs
            mapM_ (distSpaceTime spaceVec timeVec) indexPairs
            spaceVecNonMut <- VU.freeze spaceVec
            timeVecNonMut  <- VU.freeze timeVec
            return [("space", SUDistMatrix spaceVecNonMut), ("time", SUDistMatrix timeVecNonMut)]
        IndepArbitraryDimPos pos@(ArbitraryDimPos l) -> do
            arbitraryVecs <- replicateM (length l) (VUM.new nrPairs)
            mapM_ (distArbitrary arbitraryVecs) indexPairs
            arbitraryVecsNonMut <- mapM VU.freeze arbitraryVecs
            return $ zipWith (\name vec -> (name, SUDistMatrix vec)) (getKeys pos) arbitraryVecsNonMut
    where
        distSpaceTime :: VUM.IOVector Double -> VUM.IOVector Double -> (Int, (Observation, Observation)) -> IO ()
        distSpaceTime
            spaceVec timeVec
            (i,
            (Observation _ _ (HyperPos (IndepSpatTempPos p1) _),
            Observation _ _ (HyperPos (IndepSpatTempPos p2) _))
            ) = do
            let spaceDist = spatialDistSpatTempPos p1 p2 / 1000 -- scaling meters to kilometres
                timeDist  = temporalDistSpatTempPos p1 p2
            VUM.write spaceVec i spaceDist
            VUM.write timeVec  i timeDist
        distSpaceTime _ _ _ = error "Impossible state in indep distance calculation"
        distArbitrary :: [VUM.IOVector Double] -> (Int, (Observation, Observation)) -> IO ()
        distArbitrary
            arbitraryVecs
            (i,
            (Observation _ _ (HyperPos (IndepArbitraryDimPos p1) _),
            Observation _ _ (HyperPos (IndepArbitraryDimPos p2) _))
            ) = do
            -- this assumes that p1 and p2 have the same order of indep variables
            let arbitraryDists = allDistances (getValues p1) (getValues p2)
            zipWithM_ (`VUM.write` i) arbitraryVecs arbitraryDists
        distArbitrary _ _ = error "Impossible state in indep distance calculation"

calcDepVarPairwiseDistances :: [Observation] -> IO [(DepVarName, SUDistMatrix)]
calcDepVarPairwiseDistances obs = do
    let indexPairs = zip [0..] [ (x,y) | y <- obs, (x:_) <- tails obs]
        nrPairs = length indexPairs
        (Observation _ _ (HyperPos _ pos@(DepVarsPos l))) = head obs
    depVecs <- replicateM (length l) (VUM.new nrPairs)
    mapM_ (distDep depVecs) indexPairs
    depVecsNonMut <- mapM VU.freeze depVecs
    return $ zipWith (\name vec -> (name, SUDistMatrix vec)) (getKeys pos) depVecsNonMut
    where
        distDep :: [VUM.IOVector Double] -> (Int, (Observation, Observation)) -> IO ()
        distDep
            depVecs
            (i,
            (Observation _ _ (HyperPos _ p1),
            Observation _ _ (HyperPos _ p2))
            ) = do
            -- this assumes that p1 and p2 have the same order of dep variables
            let depDists = allDistances (getValues p1) (getValues p2)
            zipWithM_ (`VUM.write` i) depVecs depDists

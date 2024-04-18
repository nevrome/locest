{-# LANGUAGE BangPatterns #-}

module LocEst.CLI.Vario where

import           LocEst.Distance
import           LocEst.Parsers
import           LocEst.Types

import           Conduit                      ((.|))
import           Control.DeepSeq              (NFData)
import           Control.Monad                (replicateM, zipWithM_)
import qualified Control.Parallel.Strategies  as PS
import qualified Data.Conduit                 as Con
import qualified Data.Conduit.List            as ConL
import           Data.Function                (on)
import           Data.List                    (tails)
import           Data.Maybe                   (fromJust, mapMaybe)
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Unboxed          as VU
import qualified Data.Vector.Unboxed.Mutable  as VUM
import           System.IO                    (hPutStrLn, stderr)

data VarioOptions = VarioOptions {
    _voInObservationFile :: FilePath,
    _voVariogramOutFile  :: Maybe FilePath
}

-- helper functions for nested loops
forM :: Monad m => [a] -> (a -> m b) -> m [b]
forM = flip mapM
for :: [a] -> (a -> b) -> [b]
for = flip map
parFor :: NFData b => [a] -> (a -> b) -> [b]
parFor l f = PS.parMap PS.rdeepseq f l

runVario :: VarioOptions -> IO ()
runVario (VarioOptions inObsFile outVariogramFile) = do
    -- read observations
    hPutStrLn stderr "Reading observations"
    !observationsUnindexed <- readObservations inObsFile
    let observations = zipWith setIndex observationsUnindexed [0..]
    -- calculate pairwise distances
    hPutStrLn stderr "Calculating pairwise distances for independent variables"
    !distsPerIndepVar <- calcIndepVarPairwiseDistances observations
    hPutStrLn stderr "Calculating pairwise distances for dependent variables"
    !distsPerDepVar   <- calcDepVarPairwiseDistances observations
    -- determine bins
    hPutStrLn stderr "Determining bins for independent variables"
    let perIndepVar  = map binIndepVar distsPerIndepVar
    -- iterate over all permutations of indepVars and depVars to calculate empirical variograms
    hPutStrLn stderr "Calculating empirical variograms"
    empiricalVariograms <- fmap concat $
        -- loop over indepVars
        forM perIndepVar $ \(indepVarName, SUDistMatrix indepDists, bins) -> do
            hPutStrLn stderr ("Working on " ++ indepVarName)
            let (_,_,lastHi) = last bins
                -- remove dists that are not to be binned
                indepDistsIndexed = VU.indexed indepDists
                binnableIndepDists = VU.filter (\(_,v) -> v <= lastHi) indepDistsIndexed
            -- sort indep distance vector for easy binning
            sortedIndepDists <- sortWithIndices binnableIndepDists -- very time-consuming!
            -- get start index and stop index for each bin in the sorted indep vector
            let startLenPerBin = mapMaybe (getStartAndStopForBin sortedIndepDists) bins
            -- loop over depVars
            forM distsPerDepVar $ \(depVarName, SUDistMatrix depDists) -> do
                -- loop over bins
                let !semivariancesPerBin = parFor startLenPerBin $ \(mid, startSorted, stopSorted) ->
                            -- recover depVar values through bin indices
                        let indicesForThisBin = getIndicesForBin sortedIndepDists startSorted stopSorted
                            depDistsPerBin = VU.map (depDists VU.!) indicesForThisBin
                            -- calculate semivariance per bin
                            semivariance = calcMatheron depDistsPerBin
                        in (mid, semivariance)
                hPutStrLn stderr ("-> " ++ depVarName)
                return $ EmpiricalVariogramOneVarCombination indepVarName depVarName (EmpiricalVariogram semivariancesPerBin)
    -- write variograms to the file system
    writeVariograms empiricalVariograms outVariogramFile

-- perform binning of an indepVar
binIndepVar :: (IndepVarName, SUDistMatrix) -> (IndepVarName, SUDistMatrix, [(Double, Double, Double)])
binIndepVar (indepVarName, dist@(SUDistMatrix distVec)) =
    -- currently only supports even distance bins
    let minValue = VU.minimum distVec
        maxValue = VU.maximum distVec
        endVario = minValue + (maxValue - minValue)/3
        stepWidth = (endVario - minValue)/1000
        stepsSingle = [minValue,minValue+stepWidth..endVario]
        steps = zipWith (\lo hi -> (lo,lo+(hi-lo)/2,hi)) (init stepsSingle) (tail stepsSingle)
    in --error $ show steps
       (indepVarName, dist, steps)

-- half mean squared distance within one bin
calcMatheron :: VU.Vector Double -> Double
calcMatheron dists = (1 / (2 * n)) * VU.foldl' (\acc d -> acc + (d ** 2)) 0 dists
    where
        n = fromIntegral $ VU.length dists

-- functions to find the depVar values for indepVar bins as fast as possible
getStartAndStopForBin :: VU.Vector (Int, Double) -> (Double, Double, Double) -> Maybe (Double, Int, Int)
getStartAndStopForBin sortedVec (lo,mid,hi) = do
    startIndex <- VU.findIndex  (\(_,v) -> v >= lo) sortedVec
    stopIndex  <- VU.findIndexR (\(_,v) -> v <= hi) sortedVec
    if stopIndex < startIndex
    then Nothing
    else Just (mid, startIndex, stopIndex)
sortWithIndices :: VU.Vector (Int, Double) -> IO (VU.Vector (Int, Double))
sortWithIndices v = do
  mv <- VU.thaw v    -- Create a mutable copy
  VA.sortBy (compare `on` snd) mv -- Sort it in-place
  VU.unsafeFreeze mv -- Convert back to a pure vector
getIndicesForBin :: VU.Vector (Int, Double) -> Int -> Int -> VU.Vector Int
getIndicesForBin sortedVec i1 i2 =
    --let !_ = unsafePerformIO $ putStrLn (show i1 ++ " " ++ show (i2 - i1))
    VU.map fst $ VU.slice i1 (i2 - i1) sortedVec

-- write variograms to the file system
writeVariograms :: [EmpiricalVariogramOneVarCombination] -> Maybe FilePath -> IO ()
writeVariograms _ Nothing        = return ()
writeVariograms vars (Just path) = Con.runConduitRes $ ConL.sourceList (concatMap varToLong vars) .| sinkNamedCSV path
    where
        varToLong :: EmpiricalVariogramOneVarCombination -> [EmpiricalVariogramSingleBin]
        varToLong (EmpiricalVariogramOneVarCombination i d (EmpiricalVariogram xs)) =
            map (\(iv, dv) -> EmpiricalVariogramSingleBin i d iv dv) xs

-- distance calculation functions
calcIndepVarPairwiseDistances :: [Observation] -> IO [(IndepVarName, SUDistMatrix)]
calcIndepVarPairwiseDistances obs = do
    let indexPairs = zip [0..] [ (x,y) | y <- obs, (x:_) <- tails obs]
        nrPairs = length indexPairs
        (Observation _ _ (HyperPos indepPos _)) = head obs
    case indepPos of
        -- spatiotemporal system
        IndepSpatTempPos _ -> do
            -- create mutable vectors to write distances directly
            spaceVec <- VUM.new nrPairs
            timeVec  <- VUM.new nrPairs
            -- calculate and write distances to mutable memory
            mapM_ (distSpaceTime spaceVec timeVec) indexPairs
            -- make result vectors immutable for easier handling
            spaceVecNonMut <- VU.unsafeFreeze spaceVec
            timeVecNonMut  <- VU.unsafeFreeze timeVec
            return [("space", SUDistMatrix spaceVecNonMut), ("time", SUDistMatrix timeVecNonMut)]
        -- arbitrary dimension system
        IndepArbitraryDimPos pos@(ArbitraryDimPos l) -> do
            arbitraryVecs <- replicateM (length l) (VUM.new nrPairs)
            mapM_ (distArbitrary arbitraryVecs) indexPairs
            arbitraryVecsNonMut <- mapM VU.unsafeFreeze arbitraryVecs
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
            -- write distances to mutable vector
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
    -- writing distances to mutable vectors
    depVecs <- replicateM (length l) (VUM.new nrPairs)
    mapM_ (distDep depVecs) indexPairs
    depVecsNonMut <- mapM VU.unsafeFreeze depVecs
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

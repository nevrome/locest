{-# LANGUAGE BangPatterns #-}

module LocEst.CLI.Vario where

import           LocEst.CLI.Utils
import           LocEst.Distance
import           LocEst.Parsers
import           LocEst.Types
import           LocEst.Utils

import           Conduit                       ((.|))
import           Control.Monad                 (replicateM, zipWithM_)
import qualified Data.Conduit                  as Con
import qualified Data.Conduit.Algorithms.Async as ConAA
import qualified Data.Conduit.Combinators      as ConC
import           Data.Function                 (on)
import           Data.List                     (singleton)
import qualified Data.Vector                   as V
import qualified Data.Vector.Algorithms.Intro  as VA
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import           System.FilePath               (takeExtension)
import           System.IO                     (hPutStrLn, stderr)

data VarioOptions = VarioOptions {
      _voInObservationFile :: FilePath
    , _voSpatDistSetting   :: Maybe SpatDistSettings
    , _voInNrBins          :: BinModeSettings
    , _voInAcrossIndepVars :: Bool
    , _voInAcrossDepVars   :: Bool
    , _voInThreads         :: NumberOfThreads
    , _voVariogramOutFile  :: Maybe FilePath
}

data SpatDistSettings = SpatDistSettings {
      _sdfInSpatDistFile :: FilePath
    , _sdfNoOrderCheck   :: Bool
}

data BinModeSettings =
      BinByNrBins Int
    | BinForNugget ArbitraryDimPos
    deriving (Show)

runVario :: VarioOptions -> IO ()
runVario (VarioOptions inObsFile maybeSpatDist binModeSettings acrossIndepVars acrossDepVars threads outVariogramFile) = do
    -- number of threads
    numThreads <- setNumberOfThreads threads
    -- read observations
    observations <- readObservations inObsFile
    -- read spat dist file
    inSpatDists <- case maybeSpatDist of
        Nothing                                  -> pure Nothing
        Just (SpatDistSettings path noOrderCheck) ->
            case takeExtension path of
                ".cbor" -> Just <$> readSpatDist (ReadSpatDistDeserialise path)
                _       -> Just <$> readSpatDist (ReadSpatDistParse noOrderCheck observations Nothing path)
    -- calculate pairwise distances
    hPutStrLn stderr "Calculating pairwise distances for independent variables"
    !distsPerIndepVar <- calcIndepVarPairwiseDistances acrossIndepVars inSpatDists observations
    hPutStrLn stderr "Calculating pairwise distances for dependent variables"
    !distsPerDepVar   <- calcDepVarPairwiseDistances acrossDepVars observations
    -- iterate over all permutations of indepVars and depVars to calculate empirical variograms
    hPutStrLn stderr "Calculating empirical variograms"
    empiricalVariograms <- fmap concat $
        -- loop over indepVars
        forM distsPerIndepVar $ \(indepVarName, SUDistMatrix indepDists) -> do
            hPutStrLn stderr ("Working on " ++ indepVarName)
            -- remove dists that are not to be binned
            let minDist = VU.minimum indepDists
                maxDist = VU.maximum indepDists
                endVario = minDist + (maxDist - minDist)/3
                indepDistsIndexed = VU.indexed indepDists
                binnableIndepDists = VU.filter (\(_,v) -> v <= endVario) indepDistsIndexed
            -- sort indep distance vector for easy binning
            sortedIndepDists <- sortWithIndices binnableIndepDists -- very time-consuming!
            -- get start index and stop index for each bin in the sorted indep vector
            let startStopPerBin = case binModeSettings of
                    BinByNrBins nrBins -> binIndepVarByNrBins sortedIndepDists nrBins
                    BinForNugget thresholds -> binIndepVarForNugget sortedIndepDists thresholds indepVarName
            -- loop over depVars
            forM distsPerDepVar $ \(depVarName, SUDistMatrix depDists) -> do
                -- loop over bins
                semivariancesPerBin <- Con.runConduitRes $
                        ConC.yieldMany startStopPerBin
                        .| ConAA.asyncMapC numThreads (perBin sortedIndepDists depDists)
                        .| ConC.sinkList
                hPutStrLn stderr ("-> " ++ depVarName)
                return $ EmpiricalVariogramOneVarCombination indepVarName depVarName (EmpiricalVariogram semivariancesPerBin)
    -- write variograms to the file system
    writeVariograms empiricalVariograms outVariogramFile

-- write variograms to the file system
writeVariograms :: [EmpiricalVariogramOneVarCombination] -> Maybe FilePath -> IO ()
writeVariograms _ Nothing        = return ()
writeVariograms vars (Just path) = Con.runConduitRes $ ConC.yieldMany (concatMap varToLong vars) .| sinkNamedCSV path
    where
        varToLong :: EmpiricalVariogramOneVarCombination -> [EmpiricalVariogramSingleBin]
        varToLong (EmpiricalVariogramOneVarCombination i d (EmpiricalVariogram xs)) =
            map (\(iv, dv) -> EmpiricalVariogramSingleBin i d iv dv) xs

perBin :: VU.Vector (Int, Double) -> VU.Vector Double -> (Double, Int, Int) -> (Double, Double)
perBin sortedIndepDists depDists (mid, startSorted, stopSorted) =
    let indicesForThisBin = getIndicesForBin sortedIndepDists startSorted stopSorted
        depDistsPerBin = VU.map (depDists VU.!) indicesForThisBin
        -- calculate semivariance per bin
        semivariance = calcMatheron depDistsPerBin
    in (mid, semivariance)

-- perform binning of an indepVar
binIndepVarForNugget :: VU.Vector (Int, Double) -> ArbitraryDimPos -> IndepVarName -> [(Double, Int, Int)]
binIndepVarForNugget sortedVec (ArbitraryDimPos m) indepVarName =
    let threshold = case lookup indepVarName m of
            Nothing -> error "indep var not there"
            Just x  -> x
        stop = case VU.findIndexR (\(_,x) -> x <= threshold) sortedVec of
            Nothing -> VU.length sortedVec - 1
            Just i  -> i
    in singleton (calcMean sortedVec 0 stop, 0, stop)

binIndepVarByNrBins :: VU.Vector (Int, Double) -> Int -> [(Double, Int, Int)]
binIndepVarByNrBins sortedVec nrBins =
    let len = VU.length sortedVec
        stepWidth = len `div` nrBins -- in nr of distances
        starts = [0,stepWidth..(len - stepWidth)]
        stops = map (\x -> x-1) [stepWidth,2*stepWidth..len]
    in zipWith (\start stop -> (calcMean sortedVec start stop, start, stop)) starts stops

calcMean :: VU.Vector (Int, Double) -> Int -> Int -> Double
calcMean sortedVec start stop =
    let (_,lo) = sortedVec VU.! start
        (_,hi) = sortedVec VU.! stop
    in (lo+hi)/2

-- half mean squared distance within one bin
calcMatheron :: VU.Vector Double -> Double
calcMatheron dists = (1 / (2 * n)) * VU.foldl' (\acc d -> acc + (d ** 2)) 0 dists
    where
        n = fromIntegral $ VU.length dists

sortWithIndices :: VU.Vector (Int, Double) -> IO (VU.Vector (Int, Double))
sortWithIndices v = do
  mv <- VU.thaw v    -- Create a mutable copy
  VA.sortBy (compare `on` snd) mv -- Sort it in-place
  VU.unsafeFreeze mv -- Convert back to a pure vector
getIndicesForBin :: VU.Vector (Int, Double) -> Int -> Int -> VU.Vector Int
getIndicesForBin sortedVec i1 i2 =
    --let !_ = unsafePerformIO $ putStrLn (show i1 ++ " " ++ show (i2 - i1))
    VU.map fst $ VU.slice i1 (i2 - i1) sortedVec

makeObsPairs :: V.Vector Observation -> [(Int, (Observation, Observation))]
makeObsPairs obs =
    let obsIndexMax = V.length obs - 1
        obsPairs = [(obs V.! x, obs V.! y) | x <- [0..obsIndexMax], y <- [0..obsIndexMax], x > y]
    in zip [0..] obsPairs

-- distance calculation functions
calcIndepVarPairwiseDistances :: Bool -> Maybe SpatDistMatrix -> V.Vector Observation -> IO [(IndepVarName, SUDistMatrix)]
calcIndepVarPairwiseDistances merge maybeSpatDistMatrix obs = do
    let obsPairs = makeObsPairs obs
        nrPairs = length obsPairs
        (Observation _ _ (HyperPos indepPos _)) = V.head obs
    case (indepPos,merge) of
        -- spatiotemporal system
        (IndepSpatTempPos _,_) -> do
            -- create mutable vectors to write distances directly
            spaceVec <- VUM.new nrPairs
            timeVec  <- VUM.new nrPairs
            -- calculate and write distances to mutable memory
            mapM_ (distSpaceTime spaceVec timeVec) obsPairs
            -- make result vectors immutable for easier handling
            spaceVecNonMut <- VU.unsafeFreeze spaceVec
            timeVecNonMut  <- VU.unsafeFreeze timeVec
            return [("space", SUDistMatrix spaceVecNonMut), ("time", SUDistMatrix timeVecNonMut)]
        -- arbitrary dimension system
        (IndepArbitraryDimPos pos@(ArbitraryDimPos l),False) -> do
            arbitraryVecs <- replicateM (length l) (VUM.new nrPairs)
            mapM_ (distArbitrary arbitraryVecs) obsPairs
            arbitraryVecsNonMut <- mapM VU.unsafeFreeze arbitraryVecs
            return $ zipWith (\name vec -> (name, SUDistMatrix vec)) (getKeys pos) arbitraryVecsNonMut
        -- arbitrary dimensions merged
        (IndepArbitraryDimPos (ArbitraryDimPos _),True) -> do
            distVec <- VUM.new nrPairs
            mapM_ (distArbitraryMerged distVec) obsPairs
            distVecNonMut <- VU.unsafeFreeze distVec
            return [("all", SUDistMatrix distVecNonMut)]
    where
        distSpaceTime :: VUM.IOVector Double -> VUM.IOVector Double -> (Int, (Observation, Observation)) -> IO ()
        distSpaceTime
            spaceVec timeVec
            (i,
            (Observation i1 _ (HyperPos (IndepSpatTempPos p1) _),
            Observation i2 _ (HyperPos (IndepSpatTempPos p2) _))
            ) = do
            let timeDist  = temporalDistSpatTempPos p1 p2
                spaceDist = case maybeSpatDistMatrix of
                    Nothing             -> spatialDistSpatTempPos p1 p2 / 1000 -- scaling meters to kilometres
                    Just spatDistMatrix -> lookUpDistanceAU spatDistMatrix i1 i2
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
        distArbitraryMerged :: VUM.IOVector Double -> (Int, (Observation, Observation)) -> IO ()
        distArbitraryMerged
            distVec
            (i,
            (Observation _ _ (HyperPos (IndepArbitraryDimPos p1) _),
            Observation _ _ (HyperPos (IndepArbitraryDimPos p2) _))
            ) = do
            -- this assumes that p1 and p2 have the same order of indep variables
            let arbitraryDistEuclidean = euclideanDistance (getValues p1) (getValues p2)
            VUM.write distVec i arbitraryDistEuclidean
        distArbitraryMerged _ _ = error "Impossible state in indep distance calculation"

calcDepVarPairwiseDistances :: Bool -> V.Vector Observation -> IO [(DepVarName, SUDistMatrix)]
calcDepVarPairwiseDistances merge obs = do
    let obsPairs = makeObsPairs obs
        nrPairs = length obsPairs
        (Observation _ _ (HyperPos _ pos@(DepVarsPos l))) = V.head obs
    -- writing distances to mutable vectors
    case merge of
        False -> do
            depVecs <- replicateM (length l) (VUM.new nrPairs)
            mapM_ (distDep depVecs) obsPairs
            depVecsNonMut <- mapM VU.unsafeFreeze depVecs
            return $ zipWith (\name vec -> (name, SUDistMatrix vec)) (getKeys pos) depVecsNonMut
        True -> do
            distVec <- VUM.new nrPairs
            mapM_ (distDepMerged distVec) obsPairs
            distVecNonMut <- VU.unsafeFreeze distVec
            return [("all", SUDistMatrix distVecNonMut)]
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
        distDepMerged :: VUM.IOVector Double -> (Int, (Observation, Observation)) -> IO ()
        distDepMerged
            distVec
            (i,
            (Observation _ _ (HyperPos _ p1),
            Observation _ _ (HyperPos _ p2))
            ) = do
            -- this assumes that p1 and p2 have the same order of dep variables
            let depDistEuclidean = euclideanDistance (getValues p1) (getValues p2)
            VUM.write distVec i depDistEuclidean

{-# LANGUAGE ScopedTypeVariables #-}

module LocEst.CLI.Cross where

import           LocEst.CLI.Search             (SpaceTimeCoreSupplementSettings (SpaceTimeCoreSupplementSettings),
                                                mapOnlySearchResult)
import           LocEst.CLI.Utils
import           LocEst.CoreAlgorithms
import           LocEst.MathUtils              (foldSum, avg)
import           LocEst.Parsers
import           LocEst.Types

import           Conduit                       (ResourceT)
import           Data.Conduit                  (ConduitT, (.|))
import qualified Data.Conduit                  as Con
import qualified Data.Conduit.Algorithms.Async as ConAA
import qualified Data.Conduit.Combinators      as ConC
import qualified Data.Conduit.List             as ConL
import           Data.List                     (sortBy)
import           Data.Maybe                    (mapMaybe)
import qualified Data.Vector                   as V
import           Immutable.Shuffle             (shuffle)
import           System.FilePath               (takeExtension)
import           System.IO                     (hPutStrLn, stderr)
import           System.Random                 as R

data CrossOptions = CrossOptions
    { _crossInObservationFile  :: FilePath
    , _crossSupplementSettings :: SpaceTimeCoreSupplementSettings
    , _crossSettings           :: CrossSettings
    , _crossOutFile            :: FilePath
    , _crossOutMode            :: CrossOutModeSettings
    }

data CrossSettings = CrossSettings {
      _crossvalInKernDef    :: [KernelDefinition]
    , _crossvalTestFraction :: Double
    , _crossvalIterations   :: Int
    , _crossvalMaybeSeed    :: Maybe Int
}

data CrossOutModeSettings =
      SummedLikelihoodPerKernelSetting
    | IndividualSearchObsResults
    deriving (Show)

runCross :: CrossOptions -> Int -> IO ()
runCross (
    CrossOptions inObsFile
    spaceTimeSuppSettings
    (CrossSettings kernDefs testFraction iterations maybeSeed) outFile outMode
    ) numThreads = do
    -- read observations
    observations <- readObservations inObsFile
    -- read core supplements
    coreSupp <- readSpaceTimeSupp spaceTimeSuppSettings observations
    -- prepare permutations
    hPutStrLn stderr "Splitting test and training data"
    let numObs = fromIntegral $ length observations
        numTestObs = round $ testFraction * numObs
    seed <- case maybeSeed of
                Nothing   -> do
                    rng <- R.initStdGen
                    let (seed,_) = R.genWord32 rng
                    return $ fromIntegral seed
                Just seed -> pure seed
    let testTrainingIterations = V.map (\i -> splitTestTraining i observations numTestObs (seed + i)) (V.generate iterations id)
    -- determine nr of permutations
    let numKernDefs = length kernDefs
        numPerms = iterations * numTestObs * numKernDefs
    -- run crossvalidation pipeline
    hPutStrLn stderr "All preparations ready"
    hPutStrLn stderr "Running analysis"
    perPointRes <- Con.runConduitRes $
        -- begin to stream iterations
           ConC.yieldMany testTrainingIterations
        -- run per-iteration conduit until no iterations left
        .| Con.awaitForever (oneIterationConduit coreSupp numThreads)
        -- print progress information
        .| progress 1000 (Just numPerms)
        -- split stream to report the error cases and add the good ones to the result list
        .| mapOnlySearchResult
        .| ConC.sinkList
    case outMode of
        IndividualSearchObsResults -> do
            -- write out crossvalidation results for individual observations
            Con.runConduitRes $
                   ConC.yieldMany perPointRes-- (sortBy sortFunc perPointRes)
                .| sinkNamedCSV outFile
        SummedLikelihoodPerKernelSetting -> do
            -- summarize crossvalidation result per kernel parameter setting
            Con.runConduitRes $
                   ConC.yieldMany (sortBy sortFunc perPointRes)
                .| ConL.groupBy groupFunc
                .| ConC.map summarizeFunc
                .| sinkNamedCSV outFile
    hPutStrLn stderr "Done"
    where
        oneIterationConduit ::
               CoreSupplement
            -> Int
            -> (Int, V.Vector Observation, V.Vector Observation)
            -> ConduitT (Int, V.Vector Observation, V.Vector Observation) CoreOut (ResourceT IO) ()
        oneIterationConduit coreSupp maxNumThreads (iteration,testData,trainingData) = do
            ConC.yieldMany testData
                -- multiply multidimensional positions by algorithms
                .| ConC.concatMap (multiplyByAlgorithms iteration kernDefs)
                -- main search algorithm
                .| ConAA.asyncMapC maxNumThreads (core CoreOutFull coreSupp trainingData)
        multiplyByAlgorithms ::
               Int
            -> [KernelDefinition]
            -> Observation
            -> [CorePermutation]
        multiplyByAlgorithms iteration kernelDefs obs =
            for kernelDefs $
                \a -> CorePermutation (_hyposIndepVarsPos $ _obsPos obs) (Just $ DepVarsPredPosSearchObs obs) a 0 iteration

readSpaceTimeSupp ::
       SpaceTimeCoreSupplementSettings
    -> V.Vector Observation
    -> IO CoreSupplement
readSpaceTimeSupp
    (SpaceTimeCoreSupplementSettings
            inSpaceTimeFilter
            inSpatDistFile
            inObsTempSamplesFile
            noOrderCheck
    )
    observations = do
    hPutStrLn stderr "Reading supplements"
    -- read spatial distances
    inSpatDists <- case inSpatDistFile of
        Nothing   -> pure Nothing
        Just path -> case takeExtension path of
            ".cbor" -> Just <$> readSpatDist (ReadSpatDistDeserialise path)
            _       -> Just <$> readSpatDist (ReadSpatDistParse noOrderCheck observations Nothing path)
    -- read temporal distances
    inObsTempSamples <- case inObsTempSamplesFile of
        Nothing   -> pure Nothing
        Just path -> case takeExtension path of
            ".cbor" -> Just <$> readTempSamp (ReadTempSampDeserialise path)
            _       -> Just <$> readTempSamp (ReadTempSampParse noOrderCheck observations path)
    return $ CoreSupplement inSpaceTimeFilter inSpatDists inObsTempSamples

summarizeFunc :: [SearchResult] -> CrossvalOutput
summarizeFunc xs =
    let oneProb  = _srCorePermutation $ head xs
        kerndef  = _casKernelDefinition oneProb
        dists = mapMaybe (fmap _slhEuclideanDep  . _srLikelihood) xs
        sumDists = foldSum dists
        meanSquaredDists = avg $ map (**2) dists
        sumLogLs = foldSum $ mapMaybe (fmap _slhLogLikelihood . _srLikelihood) xs
    in CrossvalOutput kerndef sumDists meanSquaredDists sumLogLs


groupFunc :: SearchResult -> SearchResult -> Bool
groupFunc (SearchResult (CorePermutation _ _ kernDefA _ _) _ _)
          (SearchResult (CorePermutation _ _ kernDefB _ _) _ _) =
    kernDefA == kernDefB

sortFunc :: SearchResult -> SearchResult -> Ordering
sortFunc (SearchResult (CorePermutation _ _ kernDefA _ _) _ _)
         (SearchResult (CorePermutation _ _ kernDefB _ _) _ _) =
    compare kernDefA kernDefB

splitTestTraining :: Int -> V.Vector a -> Int -> Int -> (Int, V.Vector a, V.Vector a)
splitTestTraining iteration observations numTestObs seedOneIteration =
    let rng = R.mkStdGen seedOneIteration
        (observationsShuffled,_) = shuffle observations rng
        (test, training) = V.splitAt numTestObs observationsShuffled
    in (iteration, test, training)



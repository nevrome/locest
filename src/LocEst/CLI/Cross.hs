{-# LANGUAGE ScopedTypeVariables #-}

module LocEst.CLI.Cross where

import           LocEst.CLI.Search             (SpaceTimeCoreSupplementSettings (SpaceTimeCoreSupplementSettings),
                                                calculateVariances)
import           LocEst.CLI.Utils
import           LocEst.CoreAlgorithms
import           LocEst.MathUtils              (avg, foldSum)
import           LocEst.Parsers
import           LocEst.Types

import           Conduit                       (ResourceT)
import           Data.Conduit                  (ConduitT, (.|))
import qualified Data.Conduit                  as Con
import qualified Data.Conduit.Algorithms.Async as ConAA
import qualified Data.Conduit.Combinators      as ConC
import qualified Data.Conduit.List             as ConL
import           Data.List                     (sortBy, singleton, intercalate, nub)
import           Data.Maybe                    (mapMaybe)
import qualified Data.Vector                   as V
import           Immutable.Shuffle             (shuffle)
import           System.FilePath               (takeExtension)
import           System.IO                     (hPutStrLn, stderr)
import           System.Random                 as R
import Control.Monad (zipWithM)

data CrossOptions = CrossOptions
    { _crossInObservationFile  :: FilePath
    , _crossSupplementSettings :: SpaceTimeCoreSupplementSettings
    , _crossSettings           :: CrossSettings
    , _crossOutFile            :: Maybe FilePath
    , _crossOutMode            :: CrossOutModeSettings
    }

data CrossSettings = CrossSettings {
      _crossvalInKernDef        :: [[KernelOneDepVar]]
    , _crossvalCoAnalyseDepVars :: Bool
    , _crossvalInSubsetMode     :: CrossSubsetMode
    }

data CrossSubsetMode =
      CrossFull
    | CrossFraction {
      _crossvalTestFraction :: Double
    , _crossvalIterations   :: Int
    , _crossvalMaybeSeed    :: Maybe Int
    }

data CrossOutModeSettings =
      SummedLikelihoodPerKernelSetting
    | IndividualSearchObsResults
    deriving (Show)

runCross :: CrossOptions -> Int -> Double -> IO ()
runCross (
    CrossOptions inObsFile
    spaceTimeSuppSettings
    (CrossSettings kernsPerDepVar coAnalyseDepVars subsetMode) outFile outMode
    ) numThreads spatDistUnitScaling = do
    -- prepare kernel definitions
    let (kernDefs, depVars) =
            if coAnalyseDepVars
            --kernsPerDepVar: [[kernForDepVar1], [kernForDepVar2], ..
            then let kernDefs = map KernelDefinition $ sequenceA kernsPerDepVar
                     depVars = getKeys $ head kernDefs
                 in (singleton kernDefs, singleton depVars)
            else let kernDefs = map (map (KernelDefinition . singleton)) kernsPerDepVar
                     depVars = map (getKeys . head) kernDefs
                 in (kernDefs, depVars)
    -- run cross-validation for all depVars
    perDepVarPointRes <- zipWithM crossForOneDepVarCombination kernDefs depVars
    let perPointRes = concat perDepVarPointRes
    -- process cross-validation output
    case outMode of
        IndividualSearchObsResults -> do
            -- write out cross-validation results for individual observations
            Con.runConduitRes $
                   ConC.yieldMany perPointRes
                .| sinkNamedCSV outFile
        SummedLikelihoodPerKernelSetting -> do
            -- summarize cross-validation result per kernel parameter setting
            hPutStrLn stderr "Summarizing crossvalidation results"
            Con.runConduitRes $
                   ConC.yieldMany (sortBy sortFunc perPointRes)
                .| progress 100000 (Just $ length perPointRes)
                .| ConL.groupBy groupFunc
                .| ConC.map summarizeFunc
                .| sinkNamedCSV outFile
    hPutStrLn stderr "Done"
    where
        crossForOneDepVarCombination :: [KernelDefinition] -> [DepVarName] -> IO [CrossSearchResult]
        crossForOneDepVarCombination kernDefs depVars = do
            hPutStrLn stderr $ "Working on: " ++ intercalate ", " depVars
            -- list of independent variables
            let indepVars = getKeys $ _kodvLengths $ head $ _kdefPerDepVar $ head kernDefs
            -- read observations
            observationsRaw <- readObservations inObsFile
            let observations = reorderVarsInObs depVars indepVars observationsRaw
            -- variance
            hPutStrLn stderr "Calculating total variance"
            let variancesPerDepVar = calculateVariances depVars observations
            -- read core supplements
            coreSupp <- readSpaceTimeSupp spaceTimeSuppSettings observations
            -- prepare iterations
            let numKernDefs = length kernDefs
            let numObs = length observations
            -- permutation: one run of the core algorithm
            -- iteration: one test/training split
            (numberPermutations, iterations) <- case subsetMode of
                CrossFull -> do
                    hPutStrLn stderr "Prepare all-by-all prediction"
                    let nr = numKernDefs * numObs
                        allByAll = V.singleton (0, observations, observations)
                    return (nr, allByAll)
                CrossFraction testFraction iterations maybeSeed -> do
                    hPutStrLn stderr "Splitting test and training data"
                    let numTestObs = round $ testFraction * fromIntegral numObs
                        nr = numKernDefs * numTestObs * iterations
                    seed <- case maybeSeed of
                                Nothing   -> do
                                    rng <- R.initStdGen
                                    let (seed,_) = R.genWord32 rng
                                    return $ fromIntegral seed
                                Just seed -> pure seed
                    let testTrainingIterations = V.map (\i -> splitTestTraining i observations numTestObs (seed + i)) (V.generate iterations id)
                    return (nr, testTrainingIterations)
            -- run cross-validation pipeline
            hPutStrLn stderr "All preparations ready"
            hPutStrLn stderr "Running analysis"
            searchResults <- Con.runConduitRes $
                -- begin to stream iterations
                   ConC.yieldMany iterations
                -- run per-iteration conduit until no iterations left
                .| Con.awaitForever (oneIterationConduit coreSupp variancesPerDepVar numThreads depVars kernDefs)
                -- print progress information
                .| progress 1000 (Just numberPermutations)
                .| ConC.sinkList
            return $ map (CrossSearchResult depVars) searchResults
        oneIterationConduit ::
               CoreSupplement
            -> DepVarVariances
            -> Int
            -> [DepVarName]
            -> [KernelDefinition]
            -> (Int, V.Vector Observation, V.Vector Observation)
            -> ConduitT (Int, V.Vector Observation, V.Vector Observation) SearchResult (ResourceT IO) ()
        oneIterationConduit coreSupp variancesPerDepVar maxNumThreads depVars kernDefs (iteration,testData,trainingData) = do
            ConC.yieldMany testData
                -- multiply multidimensional positions by algorithms
                .| ConC.concatMap (multiplyByAlgorithms iteration kernDefs)
                -- main search algorithm
                -- operate element-by-element
                -- .| ConAA.asyncMapC maxNumThreads (coreNormal CoreOutFull variancesPerDepVar coreSupp trainingData)
                -- operate on chunks (faster and more efficient use of cores)
                .| ConC.conduitVector 1000
                .| ConAA.asyncMapC maxNumThreads (V.map (
                        coreNormal spatDistUnitScaling CoreOutFull variancesPerDepVar coreSupp depVars trainingData
                    ))
                .| ConC.concat
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
            inSpaceTimeMinFilter
            inSpaceTimeMaxFilter
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
    return $ CoreSupplement inSpaceTimeMinFilter inSpaceTimeMaxFilter inSpatDists inObsTempSamples

summarizeFunc :: [CrossSearchResult] -> CrossvalOutput
summarizeFunc xs =
    let depVars = _csrDepVars $ head xs
        oneProb = _srCorePermutation $ _csrSearchResult $ head xs
        kerndef = _casKernelDefinition oneProb
        dists   = mapMaybe (fmap _slhEuclideanDep  . _srLikelihood . _csrSearchResult) xs
        logLs   = mapMaybe (fmap _slhLogLikelihood . _srLikelihood . _csrSearchResult) xs
        sumDists         = foldSum dists
        meanSquaredDists = avg $ map (**2) dists
        sumLogLs         = foldSum logLs
    in CrossvalOutput depVars kerndef sumDists meanSquaredDists sumLogLs

groupFunc :: CrossSearchResult -> CrossSearchResult -> Bool
groupFunc (CrossSearchResult depVarA (SearchResult (CorePermutation _ _ kernDefA _ _) _ _))
          (CrossSearchResult depVarB (SearchResult (CorePermutation _ _ kernDefB _ _) _ _)) =
    depVarA == depVarB && kernDefA == kernDefB

sortFunc :: CrossSearchResult -> CrossSearchResult -> Ordering
sortFunc (CrossSearchResult depVarA (SearchResult (CorePermutation _ _ kernDefA _ _) _ _))
         (CrossSearchResult depVarB (SearchResult (CorePermutation _ _ kernDefB _ _) _ _)) =
    compare depVarA depVarB <> compare kernDefA kernDefB

splitTestTraining :: Int -> V.Vector a -> Int -> Int -> (Int, V.Vector a, V.Vector a)
splitTestTraining iteration observations numTestObs seedOneIteration =
    let rng = R.mkStdGen seedOneIteration
        (observationsShuffled,_) = shuffle observations rng
        (test, training) = V.splitAt numTestObs observationsShuffled
    in (iteration, test, training)



{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns      #-}

module LocEst.CLI.Cross where

-- import           LocEst.CoreAlgorithms
import           LocEst.Parsers
import           LocEst.Types
-- import           LocEst.Utils

-- import           Conduit                       (MonadIO (liftIO))
-- import           Data.Conduit                  ((.|))
-- import qualified Data.Conduit                  as Con
-- import qualified Data.Conduit.Algorithms.Async as ConAA
-- import qualified Data.Conduit.Combinators      as ConC
-- import qualified Data.Conduit.List             as ConL
import           Data.List                     (intercalate)
-- import           Data.Maybe                    (mapMaybe)
import qualified Data.Vector                   as V
import           Immutable.Shuffle             (shuffle)
import           System.IO                     (hPutStrLn, stderr)
import           System.Random                 as R

data CrossOptions = CrossOptions
    { _crossInObservationFile2  :: FilePath
    , _crossTestAlgorithms      :: [KernelDefinition]
    , _crossvalTestFraction2    :: Double
    , _crossvalIterations2      :: Int
    , _crossvalMaybeSeed2       :: Maybe Int
    , _crossInObsObsDistFile    :: Maybe FilePath
    , _crossOutFile2            :: Maybe FilePath
    }

runCross :: CrossOptions -> Double -> IO ()
runCross (
    CrossOptions
    inObsFile
    testAlgorithms
    testFraction iterations maybeSeed
    maybeObsObsDistFile
    outFile
    ) spatDistUnitScaling
    = do
    -- algorithm settings
    let kernelDefinition = head testAlgorithms
        algorithm = _kdefAlgorithm kernelDefinition
        depVars   = getKeys kernelDefinition
        indepVars = getKeys $ _kodvLengths $ head $ _kdefPerDepVar kernelDefinition
    hPutStrLn stderr $ "Algorithm: " ++ show algorithm
    hPutStrLn stderr $ "Dependent variables: " ++ intercalate ", " depVars
    hPutStrLn stderr $ "Independent variables: " ++ intercalate ", " indepVars
    -- read observations
    !obs <- filterVarsInObs depVars indepVars <$> readObservations inObsFile
    let nObs = V.length obs
    hPutStrLn stderr $ "Number of observations: " ++ show nObs
    -- read distances
    !obsObsDistances <- traverse (readSUDistMulti nObs) maybeObsObsDistFile
    -- splitting training and test data
    let numTestObs = round $ testFraction * fromIntegral nObs
    hPutStrLn stderr $ "Number of test observations with fraction" ++ show testFraction ++ ": " ++ show nObs
   


-- data CrossOptions = CrossOptions
--     { _crossInObservationFile  :: FilePath
--     , _crossSupplementSettings :: SupplementSettings
--     , _crossSettings           :: CrossSettings
--     , _crossOutFile            :: Maybe FilePath
--     , _crossOutMode            :: CrossOutModeSettings
--     }

-- data SupplementSettings = SupplementSettings {
--       _stcsInSpatDistFile       :: Maybe FilePath
--     , _stcsInObsTempSamplesFile :: Maybe FilePath
--     , _stcsNoOrderCheck         :: Bool
-- }

-- data CrossSettings = CrossSettings {
--       _crossvalInKernDef        :: [[KernelOneDepVar]]
--     , _crossvalCoAnalyseDepVars :: Bool
--     , _crossvalInSubsetMode     :: CrossSubsetMode
--     }

-- data CrossSubsetMode =
--       CrossFull
--     | CrossFraction {
--       _crossvalTestFraction :: Double
--     , _crossvalIterations   :: Int
--     , _crossvalMaybeSeed    :: Maybe Int
--     }

-- data CrossOutModeSettings =
--       SummedLikelihoodPerKernelSetting
--     | IndividualSearchObsResults
--     deriving (Show)

-- runCross :: CrossOptions -> Int -> Double -> IO ()
-- runCross (
--     CrossOptions inObsFile
--     crossSuppSettings
--     (CrossSettings kernsPerDepVar coAnalyseDepVars subsetMode) outFile outMode
--     ) numThreads spatDistUnitScaling = undefined
    -- prepare kernel definitions
    -- hPutStrLn stderr "Preparing kernel permutations"
    -- let (kernDefsSets, depVarsSets) =
    --         if coAnalyseDepVars
    --         --kernsPerDepVar: [[kernForDepVar1], [kernForDepVar2], ..
    --         then let ks = map makeKernelDefinition $ sequenceA kernsPerDepVar
    --                  ds = getKeys $ head ks
    --              in (singleton ks, singleton ds)
    --         else let ks = map (map (makeKernelDefinition . singleton)) kernsPerDepVar
    --                  ds = map (getKeys . head) ks
    --              in (ks, ds)
    -- read observations
    -- observationsRaw <- readObservations inObsFile
    -- count nr of permutations
    -- let numKernDefs = length $ concat kernDefsSets
    --     numObs = length observationsRaw
    --     (testFraction, numIterations) = case subsetMode of
    --         CrossFull           -> (1,1)
    --         CrossFraction f i _ -> (f,i)
    --     numTestObs = round $ testFraction * fromIntegral numObs
    --     numPermutations = numKernDefs * numTestObs * numIterations
    -- run cross-validation for all depVars
    -- Con.runConduitRes $
    --        ConC.yieldMany (zip kernDefsSets depVarsSets)
    --     .| Con.awaitForever (
    --         \(kernDefs,depVars) -> do
    --             liftIO $ hPutStrLn stderr $ "Working on: " ++ intercalate ", " depVars
                -- list of independent variables
                -- let indepVars = getKeys $ _kodvLengths $ head $ _kdefPerDepVar $ head kernDefs
                -- modify observations
                -- let observations = filterVarsInObs depVars indepVars observationsRaw
                -- read core supplements
                --coreSupp <- undefined -- liftIO $ readSupplement indepVars crossSuppSettings observationsRaw
                -- permutation: one run of the core algorithm
                -- iteration: one test/training split
                -- iterations <- case subsetMode of
                --     CrossFull -> do
                --         liftIO $ hPutStrLn stderr "Prepare all-by-all prediction"
                --         return $ V.singleton (0, observations, observations)
                --     CrossFraction _ iterations maybeSeed -> do
                --         liftIO $ hPutStrLn stderr "Splitting test and training data"
                --         seed <- case maybeSeed of
                --                     Nothing   -> do
                --                         rng <- R.initStdGen
                --                         let (seed,_) = R.genWord32 rng
                --                         return $ fromIntegral seed
                --                     Just seed -> pure seed
                --         return $ V.map (\i -> splitTestTraining i observations numTestObs (seed + i)) (V.generate iterations id)
                -- run cross-validation pipeline
                -- liftIO $ hPutStrLn stderr "Running analysis"
                -- ConC.yieldMany kernDefs
                --     .| Con.awaitForever (
                --         \kernDef ->
                --                ConC.yieldMany iterations
                --             .| ConAA.asyncMapC numThreads (
                --                 \(iteration,testData,trainingData) ->
                --                        V.map (
                --                         \obs ->
                --                             let perm = Permutation
                --                                     (_hyposIndepVarsPos $ _obsPos obs)
                --                                     (Just $ DepVarsPredPosSearchObs obs)
                --                                     kernDef 0 iteration
                --                             in coreNormal
                --                                 spatDistUnitScaling
                --                                 coreSupp
                --                                 depVars trainingData perm
                --                        ) testData
                --                ) .| ConC.concat
                --        )
                --     .| ConC.map (CrossSearchResult depVars)

    --        )
    --     .| progress 1000 (Just numPermutations)
    --     .| case outMode of
    --         IndividualSearchObsResults -> do
    --                sinkNamedCSV outFile
    --         SummedLikelihoodPerKernelSetting -> do
    --                ConL.groupBy groupFunc
    --             .| ConC.map summarizeFunc
    --             .| sinkNamedCSV outFile
    -- hPutStrLn stderr "Done"

-- readSupplement :: [String] -> SupplementSettings -> V.Vector Observation -> IO Supplement
-- readSupplement indepVarsWanted
--     (SupplementSettings
--             distanceFilterThresholdsRaw
--             inSpatDistFile
--             inObsTempSamplesFile
--             noOrderCheck
--     )
--     observations = do
--     hPutStrLn stderr "Reading supplements"
--     inSpatDists <- readMaybeSpatDist noOrderCheck observations Nothing inSpatDistFile
--     inObsTempSamples <- readMaybeObsTempSamples noOrderCheck observations inObsTempSamplesFile
--     let distanceFilterThresholds = fmap (filterDistanceThresholds indepVarsWanted) distanceFilterThresholdsRaw
--     return $ Supplement distanceFilterThresholds inSpatDists inObsTempSamples

summarizeFunc :: [CrossSearchResult] -> CrossvalOutput
summarizeFunc xs = undefined
    -- let depVars = _csrDepVars $ head xs
    --     oneProb = _srPermutation $ _csrSearchResult $ head xs
    --     kerndef = _casKernelDefinition oneProb
    --     dists   = mapMaybe (fmap _slhEuclideanDep  . _srLikelihood . _csrSearchResult) xs
    --     logLs   = mapMaybe (fmap _slhLogLikelihood . _srLikelihood . _csrSearchResult) xs
    --     sumDists         = foldSum dists
    --     meanSquaredDists = avg $ map (**2) dists
    --     sumLogLs         = foldSum logLs
    -- in CrossvalOutput depVars kerndef sumDists meanSquaredDists sumLogLs

groupFunc :: CrossSearchResult -> CrossSearchResult -> Bool
groupFunc _ _ = undefined -- TODO
--groupFunc (CrossSearchResult depVarA (SearchResult (Permutation _ _ kernDefA _ _) _ _))
--          (CrossSearchResult depVarB (SearchResult (Permutation _ _ kernDefB _ _) _ _)) =
--    depVarA == depVarB && kernDefA == kernDefB

sortFunc :: CrossSearchResult -> CrossSearchResult -> Ordering
sortFunc _ _ = undefined -- TODO
-- sortFunc (CrossSearchResult depVarA (SearchResult (Permutation _ _ kernDefA _ _) _ _))
--          (CrossSearchResult depVarB (SearchResult (Permutation _ _ kernDefB _ _) _ _)) =
--     compare depVarA depVarB <> compare kernDefA kernDefB

splitTestTraining :: Int -> V.Vector a -> Int -> Int -> (Int, V.Vector a, V.Vector a)
splitTestTraining iteration observations numTestObs seedOneIteration =
    let rng = R.mkStdGen seedOneIteration
        (observationsShuffled,_) = shuffle observations rng
        (test, training) = V.splitAt numTestObs observationsShuffled
    in (iteration, test, training)



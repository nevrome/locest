{-# LANGUAGE ScopedTypeVariables #-}

module LocEst.CLI.Cross where

import           LocEst.CLI.Search             (mapOnlyLefts, mapOnlyRights,
                                                mapOnlySearchResult,
                                                printErrors)
import           LocEst.CLI.Utils
import           LocEst.CoreAlgorithms
import           LocEst.MathUtils              (foldSum)
import           LocEst.Parsers
import           LocEst.Types
import           LocEst.Utils

import           Conduit                       (ResourceT)
import qualified Control.Monad.Except          as E
import           Data.Conduit                  (ConduitT, (.|))
import qualified Data.Conduit                  as Con
import qualified Data.Conduit.Algorithms.Async as ConAA
import qualified Data.Conduit.Combinators      as ConC
import qualified Data.Conduit.List             as ConL
import           Data.List                     (sortBy)
import           Data.Maybe                    (mapMaybe)
import qualified Data.Vector                   as V
import           Immutable.Shuffle             (shuffle)
import           System.IO                     (hPutStrLn, stderr)
import           System.Random                 as R

data CrossOptions = CrossOptions
    { _crossInObservationFile :: FilePath
    , _crossSettings          :: CrossSettings
    , _crossNumThreads        :: NumberOfThreads
    , _crossOutMode           :: CrossOutMode
    , _crossOutFile           :: FilePath
    }

data CrossSettings = CrossSettings {
      _crossvalInKernDef    :: [KernelDefinition]
    , _crossvalTestFraction :: Double
    , _crossvalIterations   :: Int
    , _crossvalMaybeSeed    :: Maybe Int
}

data CrossOutMode =
      SummedLikelihoodPerKernelSetting
    | IndividualSearchObsResults
    deriving (Show)

runCross :: CrossOptions -> IO ()
runCross (
    CrossOptions inObsFile (CrossSettings kernDefs testFraction iterations maybeSeed) threads outMode outFile
    ) = do
    -- number of threads
    numThreads <- setNumberOfThreads threads
    -- read observations
    observations <- readObservations inObsFile
    -- prepare permutations
    hPutStrLn stderr "Preparing permutations"
    -- split test and training data
    let numObs = fromIntegral $ length observations
        numTestObs = round $ testFraction * numObs
    seed <- case maybeSeed of
                Nothing   -> do
                    rng <- R.initStdGen
                    let (seed,_) = R.genWord32 rng
                    return $ fromIntegral seed
                Just seed -> pure seed
    let testTrainingIterations = V.map (\i -> splitTestTraining observations numTestObs (seed + i)) (V.generate iterations id)
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
        .| Con.awaitForever (oneIterationConduit numThreads)
        -- print progress information
        .| progress 1000 (Just numPerms)
        -- split stream to report the error cases and add the good ones to the result list
        .| Con.getZipSink (
                Con.ZipSink (
                       mapOnlyLefts
                    .| ConL.groupOn id
                    .| ConC.mapM_ printErrors
                ) *>
                Con.ZipSink (
                       mapOnlyRights
                    .| mapOnlySearchResult
                    .| ConC.sinkList
                )
           )
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
        oneIterationConduit :: Int -> (V.Vector Observation, V.Vector Observation) -> ConduitT (V.Vector Observation, V.Vector Observation) (Either LOCESTException CoreOut) (ResourceT IO) ()
        oneIterationConduit maxNumThreads (testData,trainingData) = do
            ConC.yieldMany testData
                -- multiply multidimensional positions by algorithms
                .| ConC.concatMap (multiplyByAlgorithms kernDefs)
                -- main search algorithm
                .| ConAA.asyncMapC maxNumThreads (\x -> E.runExcept (core CoreOutFull (CoreSupplement Nothing Nothing Nothing) trainingData x))

summarizeFunc :: [SearchResult] -> CrossvalOutput
summarizeFunc xs =
    let oneProb  = _srCorePermutation $ head xs
        kerndef  = _casKernelDefinition oneProb
        sumProbs = foldSum $ mapMaybe _srProbability xs
    in CrossvalOutput kerndef sumProbs

groupFunc :: SearchResult -> SearchResult -> Bool
groupFunc (SearchResult (CorePermutation _ _ kernDefA _) _ _)
          (SearchResult (CorePermutation _ _ kernDefB _) _ _) =
    kernDefA == kernDefB

sortFunc :: SearchResult -> SearchResult -> Ordering
sortFunc (SearchResult (CorePermutation _ _ kernDefA _) _ _)
         (SearchResult (CorePermutation _ _ kernDefB _) _ _) =
    compare kernDefA kernDefB

splitTestTraining :: V.Vector a -> Int -> Int -> (V.Vector a, V.Vector a)
splitTestTraining observations numTestObs seedOneIteration =
    let rng = R.mkStdGen seedOneIteration
        (observationsShuffled,_) = shuffle observations rng
    in V.splitAt numTestObs observationsShuffled

multiplyByAlgorithms ::
       [KernelDefinition]
    -> Observation
    -> [CorePermutation]
multiplyByAlgorithms
    kernelDefs
    obs =
    map (\a -> CorePermutation (_hyposIndepVarsPos $ _obsPos obs) (Just $ DepVarsPredPosSearchObs obs) a 0) kernelDefs

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocEst.CLI.Cross where

import           LocEst.CoreAlgorithms
import           LocEst.Parsers
import           LocEst.Types
import           LocEst.Utils
import LocEst.MathUtils (foldSum)
import LocEst.CLI.Search (printErrors)

import           Conduit                       (ResourceT)
import           Data.Conduit                  (ConduitT, (.|))
import qualified Data.Conduit                  as Con
import qualified Data.Conduit.Algorithms.Async as ConAA
import qualified Data.Conduit.List             as ConL
import           Data.List                     (sortBy)
import           GHC.Conc                      (getNumCapabilities)
import           System.IO (hPutStrLn, stderr)
import           System.Random                 (randomRIO)
import Data.Maybe (mapMaybe)
import qualified Control.Monad.Except as E


data CrossOptions = CrossOptions
    { _crossInObservationFile :: FilePath
    , _crossSettings          :: CrossSettings
    , _crossNumThreads        :: NumberOfThreads
    , _crossOutFile           :: FilePath
    }

data CrossSettings = CrossSettings {
      _crossvalInKernDef     :: [KernelDefinition]
    , _crossvalTestFraction  :: Double
    , _crossvalIterations    :: Int
}

runCross :: CrossOptions -> IO ()
runCross (
    CrossOptions inObsFile (CrossSettings kernDefs testFraction iterations) threads outFile
    ) = do

    -- number of threads
    numThreads <- case threads of
        SingleThread      -> pure 1
        MultipleThreads n -> pure n
        DetectThreads     -> do
            detectedThreads <- getNumCapabilities
            hPutStrLn stderr $ "Detected max number of threads: " ++ show detectedThreads
            return detectedThreads
    hPutStrLn stderr $ "Working with threads: " ++ show numThreads
    -- read observations
    hPutStrLn stderr "Reading observations"
    !observationsUnindexed <- readObservations inObsFile
    let observations = zipWith setIndex observationsUnindexed [0..]

    testTrainingIterations <- mapM (\_ -> splitTestTraining testFraction observations) [1..iterations] -- iterations could be used as seeds?

    -- run crossvalidation pipeline
    perPointRes <- Con.runConduitRes $
        -- begin to stream iterations
           ConL.sourceList testTrainingIterations
        -- run per-iteration conduit until no iterations left
        .| Con.awaitForever (oneIterationConduit numThreads)
        -- print progress information
        .| progress 1000
        -- split stream to report the error cases and add the good ones to the result list
        .| Con.getZipSink (
                Con.ZipSink (
                       ConL.mapMaybe leftToJust
                    .| ConL.groupOn id
                    .| ConL.mapM_ printErrors
                ) *>
                Con.ZipSink (
                       ConL.mapMaybe rightToJust
                    .| ConL.consume
                )
           )

    -- summary
    Con.runConduitRes $
           ConL.sourceList (sortBy sortFunc perPointRes)
        .| ConL.groupBy groupFunc
        .| ConL.map summarizeFunc
        .| sinkNamedCSV outFile

    where
        oneIterationConduit :: Int -> ([Observation],[Observation]) -> ConduitT ([Observation],[Observation]) (Either LOCESTException SearchResult) (ResourceT IO) ()
        oneIterationConduit maxNumThreads (testData,trainingData) = do
            ConL.sourceList testData
                -- multiply multidimensional positions by algorithms
                .| ConL.concatMap (multiplyByAlgorithms kernDefs)
                -- main search algorithm
                .| ConAA.asyncMapC maxNumThreads (E.runExcept . coreSearch trainingData (CoreSupplement Nothing Nothing Nothing))

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

splitTestTraining :: Double -> [a] -> IO ([a], [a])
splitTestTraining testFraction observations = do
    let numObs = fromIntegral $ length observations
    let numSamples = round $ testFraction * numObs
    observationsShuffled <- shuffle observations
    return $ splitAt numSamples observationsShuffled

-- this was written by ChatGPT after I wasted a very sad hour with the random-fu package
-- should probably be replaced with sth more reliable and faster
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
  randomIndex <- randomRIO (0, length xs - 1)
  let (left, right) = splitAt randomIndex xs
  rest <- shuffle (left ++ tail right)
  return (head right : rest)

multiplyByAlgorithms ::
       [KernelDefinition]
    -> Observation
    -> [CorePermutation]
multiplyByAlgorithms
    kernelDefs
    obs =
    map (\a -> CorePermutation (_hyposIndepVarsPos $ _obsPos obs) (Just $ _hyposDepVarsPos $ _obsPos obs) a 0) kernelDefs
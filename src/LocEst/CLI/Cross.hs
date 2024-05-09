{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocEst.CLI.Cross where

import           LocEst.CoreAlgorithms
import           LocEst.Parsers
import           LocEst.Types
import           LocEst.Utils

import           Conduit                       (ResourceT)
import           Data.Conduit                  (ConduitT, (.|))
import qualified Data.Conduit                  as Con
import qualified Data.Conduit.Algorithms.Async as ConAA
import qualified Data.Conduit.Combinators      as ConC
import qualified Data.Conduit.List             as ConL
import           Data.Either                   (isLeft)
import qualified Data.HashMap.Strict           as HM
import           Data.List                     (sort, sortBy)
import           GHC.Conc                      (getNumCapabilities)
import           System.IO (hPutStrLn, stderr)
import           System.Random                 (randomRIO)
import LocEst.CLI.Search (printErrors)

data CrossOptions = CrossOptions
    { _crossInObservationFile :: FilePath
    , _crossSettings          :: CrossSettings
    , _crossNumThreads        :: NumberOfThreads
    , _crossOutFile           :: FilePath
    }

data CrossSettings = CrossSettings {
      _crossvalTestFraction :: Double
    , _crossvalIterations   :: Int
}

runCross :: CrossOptions -> IO ()
runCross (
    CrossOptions inObsFile (CrossSettings testFraction iterations) threads outFile
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
    -- determine dependent variables
    let depVars = getKeys $ head $ map (_hyposDepVarsPos . _obsPos) observations

    testTrainingIterations <- mapM (\_ -> splitTestTraining testFraction observations) [1..iterations] -- iterations could be used as seeds?

    -- run crossvalidation pipeline
    perPointRes <- Con.runConduitRes $
        -- begin to stream iterations
           ConL.sourceList testTrainingIterations
        -- run per-iteration conduit until no iterations left
        .| Con.awaitForever (oneIterationConduit numThreads depVars)
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
        oneIterationConduit :: Int -> [String] -> ([Observation],[Observation]) -> ConduitT ([Observation],[Observation]) (Either LOCESTException SearchResult) (ResourceT IO) ()
        oneIterationConduit maxNumThreads varsOrdered (testData,trainingData) = do
            ConL.sourceList testData
                -- multiply multidimensional positions by algorithms
                .| ConL.concatMap (multiplyByAlgorithms myAlgos)
                -- main search algorithm
                .| ConAA.asyncMapC maxNumThreads (coreSearch varsOrdered trainingData Nothing)
                   -- distance grid input option not yet implemented here: Nothing

myAlgos :: [LocestAlgorithm]
myAlgos = undefined

summarizeFunc :: [SpatTempProb] -> CrossvalOutput
summarizeFunc xs =
    let oneProb  = _stprSpatTempDepVarsPosWithAlgos $ head xs
        algo     = _powialgAlgorithm oneProb
        sumProbs = sum $ map _stprprobability xs
    in CrossvalOutput algo sumProbs

groupFunc :: SpatTempProb -> SpatTempProb -> Bool
groupFunc (SpatTempProb (CorePermutation _ _ algoA _) _)
          (SpatTempProb (CorePermutation _ _ algoB _) _) =
    algoA == algoB

sortFunc :: SpatTempProb -> SpatTempProb -> Ordering
sortFunc (SpatTempProb (CorePermutation _ _ algoA _) _)
         (SpatTempProb (CorePermutation _ _ algoB _) _) =
    compare algoA algoB

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
       [LocestAlgorithm]
    -> Observation
    -> [CorePermutation]
multiplyByAlgorithms
    algorithms
    obs =
    map (\a -> CorePermutation (_hyposIndepVarsPos $ _obsPos obs) (Just $ _hyposDepVarsPos $ _obsPos obs) a 0) algorithms
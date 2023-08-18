{-# LANGUAGE ScopedTypeVariables #-}

module LocEst.CLI.Crossvalidate where

import           LocEst.CLI.Search             (printError)
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
import           System.IO                     (hPutStrLn, stderr)
import           System.Random                 (randomRIO)

data CrossvalidateOptions = CrossvalidateOptions
    { _crossvalidateInObservationFile :: FilePath
    , _crossvalidateSettings          :: CrossvalidationSettings
    , _crossvalidateOutFile           :: FilePath
    }

data CrossvalidationSettings = CrossvalidationSettings {
      _crossvalTestFraction :: Double
    , _crossvalIterations   :: Int
}

runCrossvalidate :: CrossvalidateOptions -> IO ()
runCrossvalidate (
    CrossvalidateOptions inObsFile (CrossvalidationSettings testFraction iterations) outFile
    ) = do
    allObservations <- readObservations inObsFile
    let depVarsOrdered = sort . HM.keys . getHM $ head $ map (_stpoDepVarsPos . _obsPos) allObservations

    testTrainingIterations <- mapM (\_ -> splitTestTraining testFraction allObservations) [1..iterations] -- iterations could be used as seeds?

    maxNumberOfThreads <- getNumCapabilities
    hPutStrLn stderr $ "Detected max number of threads: " ++ show maxNumberOfThreads

    -- run crossvalidation pipeline
    perPointRes <- Con.runConduitRes $
        -- begin to stream iterations
           ConL.sourceList testTrainingIterations
        -- run per-iteration conduit until no iterations left
        .| Con.awaitForever (oneIterationConduit maxNumberOfThreads depVarsOrdered)
        -- print progress information
        .| progress
        -- split stream to report the error cases and add the good ones to the result list
        .| Con.getZipSink (
                Con.ZipSink (
                       ConC.filter isLeft
                    .| ConL.mapM_ printError
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
        oneIterationConduit :: Int -> [String] -> ([Observation],[Observation]) -> ConduitT ([Observation],[Observation]) (Either LOCESTException SpatTempProb) (ResourceT IO) ()
        oneIterationConduit maxNumThreads varsOrdered (testData,trainingData) = do
            ConL.sourceList testData
                -- multiply multidimensional positions by algorithms
                .| ConL.concatMap (multiplySpatTempDepVarsPosByAlgorithms myAlgos . _obsPos)
                -- main search algorithm
                .| ConAA.asyncMapC maxNumThreads (coreSearch varsOrdered trainingData Nothing)
                   -- distance grid input option not yet implemented here: Nothing

summarizeFunc :: [SpatTempProb] -> CrossvalOutput
summarizeFunc xs =
    let oneProb  = _stprSpatTempDepVarsPosWithAlgos $ head xs
        algo     = _powialgAlgorithm oneProb
        sumProbs = sum $ map _stprprobability xs
    in CrossvalOutput algo sumProbs

groupFunc :: SpatTempProb -> SpatTempProb -> Bool
groupFunc (SpatTempProb (SpatTempDepVarsPosWithAlgorithms _ algoA) _)
          (SpatTempProb (SpatTempDepVarsPosWithAlgorithms _ algoB) _) =
    algoA == algoB

sortFunc :: SpatTempProb -> SpatTempProb -> Ordering
sortFunc (SpatTempProb (SpatTempDepVarsPosWithAlgorithms _ algoA) _)
         (SpatTempProb (SpatTempDepVarsPosWithAlgorithms _ algoB) _) =
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

multiplySpatTempDepVarsPosByAlgorithms ::
       [LocestAlgorithm]
    -> SpatTempDepVarsPos
    -> [SpatTempDepVarsPosWithAlgorithms]
multiplySpatTempDepVarsPosByAlgorithms
    algorithms
    spatTempDepVarsPos =
    map (\a -> SpatTempDepVarsPosWithAlgorithms spatTempDepVarsPos a) algorithms

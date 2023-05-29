{-# LANGUAGE ScopedTypeVariables #-}

module LocEst.CLI.Crossvalidate where

import           LocEst.Parsers
import           LocEst.Types
import           LocEst.CoreAlgorithms
import LocEst.Utils
import LocEst.CLI.Search (multiplySpatTempDepVarsPosByAlgorithms)

import           Data.Conduit                   ((.|))
import qualified Data.Conduit                   as Con
import qualified Data.Conduit.Algorithms.Async  as ConAA
import qualified Data.Conduit.List              as ConL
import qualified Data.HashMap.Strict            as HM
import qualified Data.Conduit.Combinators as ConC
import Data.List (sort)
import qualified Control.Monad as OP
import Control.Exception (throw)
import System.IO (hPutStrLn, stderr)
import GHC.Conc (getNumCapabilities)
import System.Random (randomRIO)
import           Conduit                   (MonadIO, MonadResource, ConduitT, ResourceT)

data CrossvalidateOptions = CrossvalidateOptions
    { _crossvalidateInObservationFile :: FilePath
    , _crossvalidateSettings          :: CrossvalidationSettings
    , _crossvalidateOutFile           :: FilePath
    }

data CrossvalidationSettings = CrossvalidationSettings {
      _crossvalTestFraction  :: Double
    , _crossvalIterations    :: Int
}

runCrossvalidate :: CrossvalidateOptions -> IO ()
runCrossvalidate (
    CrossvalidateOptions inObsFile (CrossvalidationSettings testFraction iterations) outFile
    ) = do
    allObservations <- readSpatTempDepVarsPos inObsFile
    let depVarsOrdered = sort . HM.keys . getHM $ head $ map _stpoDepVarsPos allObservations

    testTrainingIterations <- mapM (\_ -> splitTestTraining testFraction allObservations) [1..iterations] -- iterations could be used as seeds?

    maxNumberOfThreads <- getNumCapabilities
    hPutStrLn stderr $ "Detected max number of threads: " ++ show maxNumberOfThreads

    -- run crossvalidation pipeline
    myList <- Con.runConduitRes $
        -- begin to stream iterations
           ConL.sourceList testTrainingIterations
        -- run per-iteration conduit until no iterations left
        .| Con.awaitForever (oneIterationConduit maxNumberOfThreads depVarsOrdered)
        .| progress
        -- .| ConL.groupBy groupFunc
        .| ConL.consume

    putStrLn "done"

    where
        oneIterationConduit :: Int -> [String] -> ([SpatTempDepVarsPos],[SpatTempDepVarsPos]) -> ConduitT ([SpatTempDepVarsPos],[SpatTempDepVarsPos]) SpatTempProb (ResourceT IO) ()
        oneIterationConduit maxNumThreads varsOrdered (testData,trainingData) = do
            ConL.sourceList testData
                -- multiply multidimensional positions by algorithms
                .| ConL.concatMap (multiplySpatTempDepVarsPosByAlgorithms myDecays mySummaries)
                -- main search algorithm
                .| ConAA.asyncMapC maxNumThreads (coreSearch varsOrdered trainingData)

--groupFunc :: SpatTempProb -> SpatTempProb -> Bool
--groupFunc (SpatTempProb () _)

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
  let (left, (selected:right)) = splitAt randomIndex xs
  rest <- shuffle (left ++ right)
  return (selected : rest)
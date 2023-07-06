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
import Data.List (sort, sortBy)
import qualified Control.Monad as OP
import Control.Exception (throw)
import System.IO (hPutStrLn, stderr)
import GHC.Conc (getNumCapabilities)
import System.Random (randomRIO)
import           Conduit                   (MonadIO, MonadResource, ConduitT, ResourceT)
import qualified LocEst.Parsers as ConL

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
        .| progress
        .| ConL.consume

    -- summary
    Con.runConduitRes $
           ConL.sourceList (sortBy sortFunc perPointRes)
        .| ConL.groupBy groupFunc
        .| ConL.map summarizeFunc
        .| ConL.sinkNamedCSV outFile

    where
        oneIterationConduit :: Int -> [String] -> ([Observation],[Observation]) -> ConduitT ([Observation],[Observation]) SpatTempProb (ResourceT IO) ()
        oneIterationConduit maxNumThreads varsOrdered (testData,trainingData) = do
            ConL.sourceList testData
                -- multiply multidimensional positions by algorithms
                .| ConL.concatMap (multiplySpatTempDepVarsPosByAlgorithms myTwoDecays mySummaries . _obsPos)
                -- main search algorithm
                .| ConAA.asyncMapC maxNumThreads (coreSearch varsOrdered trainingData Nothing)
                   -- distance grid input option not yet implemented here: Nothing

myTwoDecays = [myDecay, myOtherDecay]
myOtherDecay = DecayDefinition [
      DecayOneDepVar "varC1" (LinearSum 0.0001 0.0001)
    , DecayOneDepVar "varC2" (LinearSum 0.0001 0.0001)
    ]

summarizeFunc :: [SpatTempProb] -> CrossvalOutput
summarizeFunc xs =
    let oneProb = _stprSpatTempDepVarsPosWithAlgos $ head xs
        decayDef = _powialgDecayDef oneProb
        sumAlg = _powialgDensSumAlgo oneProb
        sumProbs = sum $ map _stprprobability xs
    in CrossvalOutput decayDef sumAlg sumProbs

groupFunc :: SpatTempProb -> SpatTempProb -> Bool
groupFunc (SpatTempProb (SpatTempDepVarsPosWithAlgorithms _ decayDefA sumAlgA) _)
          (SpatTempProb (SpatTempDepVarsPosWithAlgorithms _ decayDefB sumAlgB) _) =
    (decayDefA == decayDefB) && (sumAlgA == sumAlgB)

sortFunc :: SpatTempProb -> SpatTempProb -> Ordering
sortFunc (SpatTempProb (SpatTempDepVarsPosWithAlgorithms _ decayDefA sumAlgA) _)
         (SpatTempProb (SpatTempDepVarsPosWithAlgorithms _ decayDefB sumAlgB) _) =
    mconcat [compare decayDefA decayDefB, compare sumAlgA sumAlgB]

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
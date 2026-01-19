{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns      #-}

module LocEst.CLI.Cross where

-- import           LocEst.CoreAlgorithms
import           LocEst.Parsers
import           LocEst.Types
import           LocEst.Utils

import           Conduit                       (MonadIO (liftIO))
import           Data.Conduit                  ((.|))
import qualified Data.Conduit                  as Con
import qualified Data.Conduit.Combinators      as ConC
import           Data.List                     (intercalate, foldl')
import qualified Data.Vector                   as V
import qualified Data.Vector.Storable                   as VS
import           Immutable.Shuffle             (shuffle)
import           System.IO                     (hPutStrLn, stderr)
import           System.Random                 as R
import LocEst.CLI.Search (search, Permutation (..))
import Data.Maybe (fromMaybe)

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
    testFraction iterations _ --maybeSeed
    _ -- maybeObsObsDistFile
    outFile
    ) _ --spatDistUnitScaling
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
    -- !obsObsDistances <- traverse (readSUDistMulti nObs) maybeObsObsDistFile
    -- reporting split size
    let numTestObs = round $ testFraction * fromIntegral nObs
    hPutStrLn stderr $ "Number of test observations with fraction" ++ show testFraction ++ ": " ++ show nObs
    -- 
    Con.runConduitRes $
           ConC.yieldMany [1..iterations]
        .| ConC.mapM_ (\iter ->
               Con.runConduit $
                      ConC.yieldMany testAlgorithms
                   .| ConC.concatMapM (liftIO . cross algorithm indepVars 123 numTestObs iter obs)
                   .| progress 1000 Nothing
                   .| sinkNamedCSV outFile
           )
    putStrLn "Done"

cross
  :: Algorithm
  -> [IndepVarName]
  -> Int -- base seed
  -> Int -- numTestObs
  -> Int -- iteration
  -> V.Vector Observation
  -> KernelDefinition
  -> IO [CrossvalOutput]
cross algorithm indepVars seed numTestObs iteration obs kerndef = do
    let seedIter = seed + iteration
        (testObs, trainingObs) = splitTestTraining seedIter numTestObs obs
        depVars   = getKeys kerndef
        kernels   = getValues kerndef
        -- prediction grid = test observation locations
        predGrid  = V.map posFromObs testObs
        trueVals = V.map (filterByKey depVars . depVarPosFromObs) testObs
    -- run search (no dep search grid, no temp grid)
    rows <- search algorithm indepVars Nothing Nothing Nothing 1.0 depVars kernels
                   (Permutation iteration trainingObs predGrid (Just trueVals) Nothing)
    -- align rows with true values
    let perObs = zip rows (V.toList trueVals)
        (sumDist, sumSqDist, sumLL, n) = foldl' step (0,0,0,0 :: Int) perObs
        step (!sd,!ssd,!sll,!k) (row, trueDV) =
            let predDV = makeValuesPerDepVar $ zip (_ssrDepVarName row) (_ssrMedian row)
                d      = depEuclidean predDV trueDV
                ll     = fromMaybe 0 (_ssrGridAggLogLik row)
            in (sd  + d, ssd + d*d, sll + ll, k + 1)
        meanSq
          | n == 0    = 0
          | otherwise = sumSqDist / fromIntegral n
    -- prepare output
    return
      [ CrossvalOutput
          { _crossoutDepVars          = depVars
          , _crossoutKernelDefinition = kerndef
          , _crossoutDistSum          = sumDist
          , _crossoutDistMeanSquared  = meanSq
          , _crossoutProbSum          = sumLL
          }
      ]

splitTestTraining :: Int -> Int -> V.Vector a -> (V.Vector a, V.Vector a)
splitTestTraining seedOneIteration numTestObs observations =
    let rng = R.mkStdGen seedOneIteration
        (observationsShuffled,_) = shuffle observations rng
        (test, training) = V.splitAt numTestObs observationsShuffled
    in (test, training)

depEuclidean :: DepVarsPos -> DepVarsPos -> Double
depEuclidean (ValuesPerDepVar _ v1) (ValuesPerDepVar _ v2) =
    sqrt . VS.sum $ VS.zipWith (\x y -> (x - y)^(2 :: Int)) v1 v2


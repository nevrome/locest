{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocEst.CLI.Cross where

import           LocEst.Distance          (depEuclidean)
import           LocEst.Parsers
import           LocEst.Types
import           LocEst.TypesFlat
import           LocEst.Utils

import           Conduit                  (MonadIO (liftIO))
import           Data.Conduit             ((.|))
import qualified Data.Conduit             as Con
import qualified Data.Conduit.Combinators as ConC
import           Data.List                (foldl', intercalate, nub)
import           Data.Maybe               (fromMaybe)
import qualified Data.Vector              as V
import qualified Data.Vector.Storable     as VS
import           Immutable.Shuffle        (shuffle)
import           LocEst.CLI.Search        (Permutation (..), search)
import           System.IO                (hPutStrLn, stderr)
import           System.Random            as R

data CrossOptions = CrossOptions
    { _crossInObservationFile :: FilePath
    , _crossTestAlgorithms    :: [KernelDefinition]
    , _crossTestFraction      :: Double
    , _crossIterations        :: Int
    , _crossMaybeSeed         :: Maybe Int
    , _crossInObsObsDistFile  :: Maybe FilePath
    , _crossOutFile           :: Maybe FilePath
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
    let firstKernel = head testAlgorithms
        algorithm = _kdefAlgorithm firstKernel
        depVars   = nub $ concatMap getKeys testAlgorithms -- here we have to check every kernel
        indepVars = getKeys $ _kodvLengths $ head $ _kdefPerDepVar firstKernel
    hPutStrLn stderr $ "Algorithm: " ++ show algorithm
    hPutStrLn stderr $ "Dependent variables: " ++ intercalate ", " depVars
    hPutStrLn stderr $ "Independent variables: " ++ intercalate ", " indepVars
    -- read observations
    !obs <- filterVarsInObs depVars indepVars <$> readObservations inObsFile
    let nObs = V.length obs
    hPutStrLn stderr $ "Number of observations: " ++ show nObs
    -- read distances
    !obsObsDistances <- traverse (readSUDistMulti nObs) maybeObsObsDistFile
    -- reporting split size
    hPutStrLn stderr $ "Requested split fraction: " ++ show testFraction
    let numTestObs = round $ testFraction * fromIntegral nObs
    hPutStrLn stderr $ "Number of test observations: " ++ show numTestObs
    -- set base seed
    baseSeed <- case maybeSeed of
        Just x  -> pure x
        Nothing -> R.randomRIO (0, maxBound :: Int)
    hPutStrLn stderr $ "Seed for random splitting: " ++ show baseSeed
    -- determine steps
    hPutStrLn stderr "Preparing permutations"
    let permutations = [ (iter, kerndef) | iter <- [1..iterations], kerndef <- testAlgorithms ]
    hPutStrLn stderr $ "Number of requested iterations: " ++ show iterations
    hPutStrLn stderr $ "Number of test kernel permutations: " ++ show (length testAlgorithms)
    -- run crossvalidation
    Con.runConduitRes $
           ConC.yieldMany permutations
        .| progress 1 (Just (length permutations))
        .| ConC.mapM (\(iter, kerndef) ->
               liftIO $ cross algorithm indepVars obsObsDistances spatDistUnitScaling
                              baseSeed numTestObs iter obs kerndef
           )
        .| sinkNamedCSV outFile
    putStrLn "Done"

cross
  :: Algorithm
  -> [IndepVarName]
  -> Maybe SUDistMatrixPerIndepVar
  -> Double
  -> Int -- base seed
  -> Int -- numTestObs
  -> Int -- iteration
  -> V.Vector Observation
  -> KernelDefinition
  -> IO CrossvalOutput
cross algorithm indepVars maybeFullObsObsDists spatDistUnitScaling seed numTestObs iter obs kerndef = do
    let seedIter = seed + iter
        depVars  = getKeys kerndef
        kernels  = getValues kerndef
        -- randomly slice training and test
        (testIdx, trainIdx) = splitIdx seedIter numTestObs (V.length obs)
        testObs     = V.backpermute obs (V.convert testIdx)
        trainingObs = V.backpermute obs (V.convert trainIdx)
        -- prediction grid = test observation locations
        predGrid = V.map posFromObs testObs
        trueVals = V.map (filterByKey depVars . depVarPosFromObs) testObs
        -- slice distance matrices, if present
        !maybeObsObsDists = sliceSUDistPerIndep trainIdx <$> maybeFullObsObsDists
        !maybeGridGridDists = sliceSUDistPerIndep testIdx <$> maybeFullObsObsDists
        !maybeObsGridDists = sliceAUDistPerIndep testIdx trainIdx <$> maybeFullObsObsDists
    -- run search (no dep search grid, no temp grid, but true values for grid pos)
    rows <- search algorithm indepVars maybeObsGridDists maybeObsObsDists maybeGridGridDists
                   spatDistUnitScaling depVars kernels
                   (Permutation iter trainingObs predGrid (Just trueVals) Nothing)
    -- compute summary statistics
    let perObs = zip rows (V.toList trueVals)
        (sumDist, sumSqDist, sumLL, n) = foldl' step (0,0,0,0 :: Int) perObs
        step (!sd,!ssd,!sll,!k) (row, trueDV) =
            let predDV = makeValuesPerDepVar $ zip (_ssrDepVarName row) (_ssrMedian row)
                d      = depEuclidean predDV trueDV
                ll     = fromMaybe (-inf) (_ssrGridAggLogLik row)
            in (sd + d, ssd + d*d, sll + ll, k + 1)
        meanSq
          | n == 0    = 0
          | otherwise = sumSqDist / fromIntegral n
    -- prepare output
    return CrossvalOutput
      { _crossoutIteration        = iter
      , _crossoutDepVars          = depVars
      , _crossoutKernelDefinition = kerndef
      , _crossoutDistSum          = sumDist
      , _crossoutDistMeanSquared  = meanSq
      , _crossoutProbSum          = sumLL
      }

splitIdx :: Int -> Int -> Int -> (VS.Vector Int, VS.Vector Int)
splitIdx seed nTest n =
  let rng = R.mkStdGen seed
      idxs = V.fromList [0..n-1]
      (shuffled,_) = shuffle idxs rng
  in VS.splitAt nTest (VS.convert shuffled)

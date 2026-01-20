{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocEst.CLI.Cross where

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
    { _crossInObservationFile2 :: FilePath
    , _crossTestAlgorithms     :: [KernelDefinition]
    , _crossvalTestFraction2   :: Double
    , _crossvalIterations2     :: Int
    , _crossvalMaybeSeed2      :: Maybe Int
    , _crossInObsObsDistFile   :: Maybe FilePath
    , _crossOutFile2           :: Maybe FilePath
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
        depVars   = nub $ concatMap getKeys testAlgorithms
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
    -- reporting split size
    let numTestObs = round $ testFraction * fromIntegral nObs
    hPutStrLn stderr $ "Number of test observations with fraction " ++ show testFraction ++ ": " ++ show numTestObs
    -- set base seed
    baseSeed <- case maybeSeed of
        Just x  -> pure x
        Nothing -> R.randomRIO (0, maxBound :: Int)
    -- determine steps
    let work = [ (iter, kerndef) | iter <- [1..iterations], kerndef <- testAlgorithms ]
    hPutStrLn stderr $ "Number of iterations " ++ show iterations
    hPutStrLn stderr $ "Number of test kernel permutations " ++ show (length testAlgorithms)
    Con.runConduitRes $
           ConC.yieldMany work
        .| progress 1 (Just (length work))
        .| ConC.concatMapM (\(iter, kerndef) ->
               liftIO $ cross spatDistUnitScaling algorithm indepVars obsObsDistances
                              baseSeed numTestObs iter obs kerndef
           )
        .| sinkNamedCSV outFile
    putStrLn "Done"

cross
  :: Double
  -> Algorithm
  -> [IndepVarName]
  -> Maybe SUDistMatrixPerIndepVar
  -> Int -- base seed
  -> Int -- numTestObs
  -> Int -- iteration
  -> V.Vector Observation
  -> KernelDefinition
  -> IO [CrossvalOutput]
cross spatDistUnitScaling algorithm indepVars maybeFullObsObsDists seed numTestObs iter obs kerndef = do
    let seedIter = seed + iter
        depVars   = getKeys kerndef
        kernels   = getValues kerndef
        -- randomly slice training and test
        (testIdx, trainIdx) = splitIdx seedIter numTestObs (V.length obs)
        testObs     = V.backpermute obs (V.convert testIdx)
        trainingObs = V.backpermute obs (V.convert trainIdx)
        -- prediction grid = test observation locations
        predGrid  = V.map posFromObs testObs
        trueVals = V.map (filterByKey depVars . depVarPosFromObs) testObs
        -- slice distance matrices, if present
        !maybeObsObsDists = sliceSUDistPerIndep trainIdx <$> maybeFullObsObsDists
        !maybeGridGridDists = sliceSUDistPerIndep testIdx <$> maybeFullObsObsDists
        !maybeObsGridDists = sliceAUDistPerIndep testIdx trainIdx <$> maybeFullObsObsDists
    -- run search (no dep search grid, no temp grid)
    rows <- search algorithm indepVars maybeObsGridDists maybeObsObsDists maybeGridGridDists spatDistUnitScaling depVars kernels
                   (Permutation iter trainingObs predGrid (Just trueVals) Nothing)
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
          { _crossoutIteration        = iter
          , _crossoutDepVars          = depVars
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

splitIdx :: Int -> Int -> Int -> (VS.Vector Int, VS.Vector Int)
splitIdx seed nTest n =
  let rng = R.mkStdGen seed
      idxs = V.fromList [0..n-1]
      (shuffled,_) = shuffle idxs rng
  in VS.splitAt nTest (VS.convert shuffled)

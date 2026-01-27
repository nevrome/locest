{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocEst.CLI.Cross where

import           LocEst.Parsers
import           LocEst.Types
import           LocEst.TypesFlat
import           LocEst.Utils

import           Conduit                  (MonadIO (liftIO))
import qualified Control.Monad            as OP
import           Control.Monad.ST         (runST)
import           Data.Conduit             ((.|))
import qualified Data.Conduit             as Con
import qualified Data.Conduit.Combinators as ConC
import           Data.List                (intercalate, nub)
import qualified Data.List.NonEmpty       as N
import           Data.Maybe               (fromMaybe)
import qualified Data.Vector              as V
import qualified Data.Vector.Mutable      as VM
import qualified Data.Vector.Storable     as VS
import           LocEst.CLI.Search        (Permutation (..), searchPerDepVar)
import           System.IO                (hPutStrLn, stderr)
import           System.Random            as R

data CrossOptions = CrossOptions
    { _crossInObservationFile :: FilePath
    , _crossTestAlgorithms    :: N.NonEmpty KernelDefinition
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
    let firstKernel = N.head testAlgorithms
        algorithm = _kdefAlgorithm firstKernel
        depVars   = nub $ concatMap getKeys testAlgorithms -- here we have to check every kernel
        indepVars = case _kdefPerDepVar firstKernel of
            (k:_) -> getKeys (_kodvLengths k)
            []    -> throwL "runSearch: empty KernelDefinition (this should be impossible)"
    hPutStrLn stderr $ "Algorithm: " ++ show algorithm
    hPutStrLn stderr $ "Dependent variables: " ++ intercalate ", " depVars
    hPutStrLn stderr $ "Independent variables: " ++ intercalate ", " indepVars
    -- read observations
    !obs <- filterVarsInObs depVars indepVars <$> readObservations inObsFile
    let nObs = V.length obs
    hPutStrLn stderr $ "Number of observations: " ++ show nObs
    -- read distances
    !obsObsDistances <- traverse (readSelfDistMulti nObs) maybeObsObsDistFile
    -- reporting split size
    hPutStrLn stderr $ "Requested split fraction: " ++ show testFraction
    let numTestObs = round $ testFraction * fromIntegral nObs
    hPutStrLn stderr $ "Number of test observations: " ++ show numTestObs
    OP.when (numTestObs == nObs) $ do
        hPutStrLn stderr "The number of test observations equals the number of observations. \
                         \In this special case the training set is also set to include all observations."
    -- set base seed
    baseSeed <- case maybeSeed of
        Just x  -> pure x
        Nothing -> R.randomRIO (0, maxBound :: Int)
    hPutStrLn stderr $ "Seed for random splitting: " ++ show baseSeed
    -- determine steps
    hPutStrLn stderr "Preparing permutations"
    let permutations = [ (iter, kerndef) | iter <- [1..iterations], kerndef <- N.toList testAlgorithms ]
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
  -> Maybe SelfDistMatrixPerIndepVar
  -> Double
  -> Int -- base seed
  -> Int -- numTestObs
  -> Int -- iteration
  -> V.Vector Observation
  -> KernelDefinition
  -> IO CrossvalOutput
cross algorithm indepVars maybeFullObsObsDists spatDistUnitScaling seed nTestObs iter obs kernDef = do
    let seedIter = seed + iter
        depVars = getKeys kernDef
        oneDepVar = case depVars of
            [x] -> x
            _   -> throwL "cross: expected exactly one dependent variable"
        kernels = getValues kernDef
        nObs    = V.length obs
        (testIdx, trainIdx)
          -- full autoprediction
          | nTestObs == nObs = (VS.fromList [0 .. nObs - 1], VS.fromList [0 .. nObs - 1])
          | otherwise = splitIdx seedIter nTestObs nObs
        testObs     = V.backpermute obs (V.convert testIdx)
        trainingObs = V.backpermute obs (V.convert trainIdx)
        -- prediction grid = locations of test observations
        predGrid = V.map posFromObs testObs
        -- true dependent-variable values at grid points
        trueVals = V.map (filterByKey [oneDepVar] . depVarPosFromObs) testObs
        -- slice distance matrices if provided
        !maybeObsObsDists = sliceSelfDistPerIndep trainIdx <$> maybeFullObsObsDists
        !maybeObsGridDists = sliceCrossDistPerIndep testIdx trainIdx <$> maybeFullObsObsDists
    -- run interpolation (no dep-search grid, but true grid values provided)
    perDepVar <- searchPerDepVar algorithm 0 indepVars maybeObsGridDists maybeObsObsDists
                                 spatDistUnitScaling [oneDepVar] kernels
                                 (Permutation iter trainingObs predGrid (Just trueVals) Nothing)
    -- extract the single dependent-variable result vector
    depRes <- case perDepVar of
        [v] -> pure v
        _   -> throwL "cross: expected exactly one SearchResultLong vector"
    -- accumulate CV statistics
    let (sumSqErr, sumLL, n) =  V.ifoldl' step (0, 0, 0 :: Int) depRes
        step (!sse, !sll, !k) i ssl =
            let trueDV   = trueVals V.! i
                trueVal  = lookupUnsafe trueDV oneDepVar
                medianV  = _sslMedian ssl
                d        = medianV - trueVal
                ll       = fromMaybe (-inf) (_sslGridLogLikelihood ssl)
            in (sse + d * d, sll + ll, k + 1)
    pure CrossvalOutput
      { _crossoutIteration        = iter
      , _crossoutDepVars          = [oneDepVar]
      , _crossoutKernelDefinition = kernDef
      , _crossoutDistSum          = sumSqErr
      , _crossoutDistMeanSquared  =  if n == 0 then 0 else sumSqErr / fromIntegral n
      , _crossoutProbSum          = sumLL
      }

splitIdx :: Int -> Int -> Int -> (VS.Vector Int, VS.Vector Int)
splitIdx seed nTest n =
  let rng = R.mkStdGen seed
      idxs = V.fromList [0..n-1]
      (shuffled,_) = shuffle idxs rng
  in VS.splitAt nTest (VS.convert shuffled)

shuffle :: V.Vector a -> R.StdGen -> (V.Vector a, R.StdGen)
shuffle vec0 gen0 =
  let n = V.length vec0
  in runST $ do
       mv <- V.thaw vec0
       let go !i !gen
             | i <= 1 = do
                 v <- V.freeze mv
                 pure (v, gen)
             | otherwise = do
                 let (j, gen') = R.randomR (0, i-1) gen
                 VM.swap mv (i-1) j
                 go (i-1) gen'
       go n gen0

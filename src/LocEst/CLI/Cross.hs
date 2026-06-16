{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocEst.CLI.Cross where

import           LocEst.CLI.Search
import           LocEst.Distributions
import           LocEst.Parsers
import           LocEst.Types
import           LocEst.TypesFlat
import           LocEst.Utils

import           Conduit                  (MonadIO (liftIO))
import qualified Control.Monad            as OP
import           Data.Conduit             ((.|))
import qualified Data.Conduit             as Con
import qualified Data.Conduit.Combinators as ConC
import           Data.List                (intercalate, nub)
import qualified Data.List.NonEmpty       as N
import qualified Data.Vector              as V
import qualified Data.Vector.Storable     as VS
import           System.IO                (hPutStrLn, stderr)
import           System.Random            as R

data CrossOptions = CrossOptions
    { _crossInObservationFile :: FilePath
    , _crossTestAlgorithms    :: N.NonEmpty KernelDefinition
    , _crossFolds             :: Word
    , _crossIterations        :: Word
    , _crossMaybeSeed         :: Maybe Int
    , _crossInObsObsDistFile  :: Maybe FilePath
    , _crossOutFile           :: Maybe FilePath
    }

runCross :: CrossOptions -> Double -> IO ()
runCross (
    CrossOptions
    inObsFile
    testAlgorithms
    folds' iterations' maybeSeed
    maybeObsObsDistFile
    outFile
    ) spatDistUnitScaling
    = do
    -- algorithm settings
    let folds      = fromIntegral folds'
        iterations = fromIntegral iterations'
        kdefs      = N.toList testAlgorithms
        firstKDef  = N.head testAlgorithms
        algorithm  = _kdefAlgorithm firstKDef
        depVars    = nub $ concatMap getKeys kdefs
        indepVars  = nub $ concatMap kernelIndepVars kdefs
        nKDefs     = length kdefs
        nrWorkItems = iterations * folds * nKDefs
    hPutStrLn stderr $ "Algorithm: " ++ show algorithm
    hPutStrLn stderr $ "Dependent variables: " ++ intercalate ", " depVars
    hPutStrLn stderr $ "Independent variables: " ++ intercalate ", " indepVars
    -- read observations
    !obs <- filterVarsInObs depVars indepVars <$> readObservations inObsFile
    let nObs = V.length obs
    hPutStrLn stderr $ "Number of observations: " ++ show nObs
    -- read distances
    !obsObsDistances <- traverse (readSelfDistMulti nObs) maybeObsObsDistFile
    -- k-fold settings
    hPutStrLn stderr $ "Number of requested iterations: " ++ show iterations
    hPutStrLn stderr $ "Number of folds per iteration: " ++ show folds
    OP.when (folds == 1) $ hPutStrLn stderr "--folds was set to 1. In this special case the training set includes all observations."
    hPutStrLn stderr $ "Number of test kernel permutations: " ++ show (length testAlgorithms)
    hPutStrLn stderr $ "Effective test fraction per fold: " ++ show (1 / fromIntegral folds :: Double)
    -- set base seed
    baseSeed <- case maybeSeed of
        Just x  -> pure x
        Nothing -> R.randomRIO (0, maxBound :: Int)
    hPutStrLn stderr $ "Seed for random splitting: " ++ show baseSeed
    -- run crossvalidation
    Con.runConduitRes $
           ConC.yieldMany [1 .. iterations]
        .| ConC.concatMap (\iter -> [ (iter, fold, kernDef) | fold <- [1 .. folds], kernDef <- kdefs])
        .| ConC.mapM (\(iter, fold, kernDef) ->
             liftIO $ cross
                 spatDistUnitScaling
                 obsObsDistances
                 baseSeed
                 iter
                 folds
                 fold
                 obs
                 kernDef
             )
        .| progress 1 (Just nrWorkItems)
        .| sinkNamedCSV outFile
    hPutStrLn stderr "Done"

kernelIndepVars :: KernelDefinition -> [IndepVarName]
kernelIndepVars kernDef =  case _kdefPerDepVar kernDef of
    k:_ -> getKeys (_kodvLengths k)
    []  -> throwL "kernelIndepVars: empty KernelDefinition"

cross
    :: Double
    -> Maybe SelfDistMatrixPerIndepVar
    -> Int -- base seed
    -> Int -- iteration
    -> Int -- number of folds
    -> Int -- fold, 1-based
    -> V.Vector Observation
    -> KernelDefinition
    -> IO CrossvalOutput
cross spatDistUnitScaling maybeFullObsObsDists seed iter folds fold obs kernDef = do
    let algorithm = _kdefAlgorithm kernDef
        indepVars = kernelIndepVars kernDef
        depVars   = getKeys kernDef
        oneDepVar = case depVars of
            [x] -> x
            _   -> throwL "cross: expected exactly one dependent variable"
        kernels   = getValues kernDef
        nObs      = V.length obs
        seedIter  = seed + iter
        (testIdx, trainIdx) = kFoldIdx seedIter folds fold nObs
        testObs     = V.backpermute obs (V.convert testIdx)
        trainingObs = V.backpermute obs (V.convert trainIdx)
        -- prediction grid = locations of test observations
        predGrid = V.map posFromObs testObs
        -- true dependent-variable values at grid points
        trueVals = V.map (filterByKey [oneDepVar] . depVarPosFromObs) testObs
        -- slice distance matrices if provided
        !maybeObsObsDists  = sliceSelfDistPerIndep trainIdx <$> maybeFullObsObsDists
        !maybeObsGridDists = sliceCrossDistPerIndep testIdx trainIdx <$> maybeFullObsObsDists
    -- run interpolation (no dep-search grid, but true grid values provided)
    perDepVar <- interpolPerDepVar
        spatDistUnitScaling
        algorithm
        0
        indepVars
        maybeObsGridDists
        maybeObsObsDists
        [oneDepVar]
        kernels
        trainingObs
        predGrid
        (Just trueVals)
    depRes <- case perDepVar of
        [v] -> pure v
        _   -> throwL "cross: expected exactly one InterpolResultLong vector"
    let (sumSqErr, sumLL, n) = V.ifoldl' step (0, 0, 0 :: Int) depRes
        step (!sse, !sll, !k) i irl =
            let trueVal = lookupUnsafe (trueVals V.! i) oneDepVar
                medianV = either (const nan) (`predQuantile` 0.5) (_irlPredDist irl)
                d = medianV - trueVal
                ll = either (const (-inf)) (`predLogDensity` trueVal) (_irlPredDist irl)
            in (sse + d * d, sll + ll, k + 1)
    pure CrossvalOutput
      { _crossoutIteration        = iter
      , _crossoutFold             = fold
      , _crossoutDepVars          = oneDepVar
      , _crossoutKernelDefinition = kernDef
      , _crossoutDistSum          = sumSqErr
      , _crossoutDistMeanSquared  = if n == 0 then 0 else sumSqErr / fromIntegral n
      , _crossoutProbSum          = sumLL
      }

kFoldIdx :: Int -> Int -> Int -> Int -> (VS.Vector Int, VS.Vector Int)
kFoldIdx seed k fold1 n
    | k < 1 = throwL "kFoldIdx: number of folds must be at least 1"
    | fold1 < 1 || fold1 > k = throwL "kFoldIdx: fold index out of range"
    | k == 1 =
        -- full autoprediction
        let allIdx = VS.fromList [0 .. n - 1]
        in (allIdx, allIdx)
    | k > n = throwL "kFoldIdx: number of folds cannot exceed number of observations"
    | otherwise =
        let rng = R.mkStdGen seed
            idxs = V.fromList [0 .. n - 1]
            (shuffled, _) = shuffle idxs rng
            perm = VS.convert shuffled
            fold0 = fold1 - 1
            -- distribute the remainder over the first folds
            -- example: n = 10, k = 3 gives sizes 4, 3, 3
            (q, r) = n `quotRem` k
            foldSize f = q + if f < r then 1 else 0
            foldStart f = f * q + min f r
            start = foldStart fold0
            len   = foldSize fold0
            (before, rest) = VS.splitAt start perm
            (test, after)  = VS.splitAt len rest
            train          = VS.concat [before, after]
        in (test, train)

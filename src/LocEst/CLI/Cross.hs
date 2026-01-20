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
import LocEst.TypesFlat

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
    testFraction iterations maybeSeed
    maybeObsObsDistFile
    outFile
    ) spatDistUnitScaling
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
    !obsObsDistances <- traverse (readSUDistMulti nObs) maybeObsObsDistFile
    -- reporting split size
    let numTestObs = round $ testFraction * fromIntegral nObs
    hPutStrLn stderr $ "Number of test observations with fraction" ++ show testFraction ++ ": " ++ show nObs
    -- set base seed
    baseSeed <- case maybeSeed of
        Just x -> pure x
        Nothing -> R.randomRIO (0, maxBound :: Int)
    -- run crossvalidation iterations
    Con.runConduitRes $
           ConC.yieldMany [1..iterations]
        .| ConC.mapM_ (\iter ->
               Con.runConduit $
                      ConC.yieldMany testAlgorithms
                   .| ConC.concatMapM (liftIO . cross spatDistUnitScaling algorithm indepVars obsObsDistances baseSeed numTestObs iter obs)
                   .| progress 1000 Nothing
                   .| sinkNamedCSV outFile
           )
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
cross spatDistUnitScaling algorithm indepVars maybeFullObsObsDists seed numTestObs iteration obs kerndef = do
    let seedIter = seed + iteration
        depVars   = getKeys kerndef
        kernels   = getValues kerndef
        -- randomly slice training and test
        (testIdx, trainIdx) = splitIdx seedIter numTestObs (V.length obs)
        testObs     = V.backpermute obs testIdx
        trainingObs = V.backpermute obs trainIdx
        -- prediction grid = test observation locations
        predGrid  = V.map posFromObs testObs
        trueVals = V.map (filterByKey depVars . depVarPosFromObs) testObs
        -- slice distance matrices, if present
        !maybeObsObsDists = sliceSUDistPerIndep trainIdx <$> maybeFullObsObsDists
        !maybeGridGridDists = sliceSUDistPerIndep testIdx <$> maybeFullObsObsDists
        !maybeObsGridDists = sliceAUDistPerIndep testIdx trainIdx <$> maybeFullObsObsDists
    -- run search (no dep search grid, no temp grid)
    rows <- search algorithm indepVars maybeObsGridDists maybeObsObsDists maybeGridGridDists spatDistUnitScaling depVars kernels
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

splitIdx :: Int -> Int -> Int -> (V.Vector Int, V.Vector Int)
splitIdx seed nTest n =
  let rng = R.mkStdGen seed
      idxs = V.fromList [0..n-1]
      (shuffled,_) = shuffle idxs rng
  in V.splitAt nTest shuffled

{-# INLINE idxHalf #-}
idxHalf :: Int -> Int -> Int
idxHalf i j
  | i >= j    = i*(i+1) `div` 2 + j
  | otherwise = j*(j+1) `div` 2 + i

sliceSUDistMatrix
    :: V.Vector Int
    -> SUDistMatrix
    -> SUDistMatrix
sliceSUDistMatrix idxs (SUDistMatrix full) =
  let n = V.length idxs
      nHalf = n*(n+1) `div` 2
      v = VS.generate nHalf $ \k ->
            let (i,j) = unHalf k
                oi = idxs V.! i
                oj = idxs V.! j
            in full VS.! idxHalf oi oj
  in SUDistMatrix v
  where
    unHalf k =
      let i = floor ((sqrt (8*fromIntegral k + 1) - 1) / 2)
          j = k - i*(i+1) `div` 2
      in (i,j)

sliceAUDistMatrix
  :: V.Vector Int   -- test indices (rows)
  -> V.Vector Int   -- train indices (cols)
  -> SUDistMatrix   -- full obs–obs
  -> AUDistMatrix
sliceAUDistMatrix testIdx trainIdx (SUDistMatrix full) =
  let nGrid = V.length testIdx
      nObs  = V.length trainIdx
      v = VS.generate (nGrid * nObs) $ \k ->
            let (g,o) = k `divMod` nObs
                oi = trainIdx V.! o
                gi = testIdx  V.! g
            in full VS.! idxHalf oi gi
  in AUDistMatrix nObs nGrid v

sliceSUDistPerIndep
  :: V.Vector Int
  -> SUDistMatrixPerIndepVar
  -> SUDistMatrixPerIndepVar
sliceSUDistPerIndep idxs (SUDistMatrixPerIndepVar ms) =
  SUDistMatrixPerIndepVar
    [ (name, sliceSUDistMatrix idxs m) | (name,m) <- ms ]

sliceAUDistPerIndep
  :: V.Vector Int
  -> V.Vector Int
  -> SUDistMatrixPerIndepVar
  -> AUDistMatrixPerIndepVar
sliceAUDistPerIndep testIdx trainIdx (SUDistMatrixPerIndepVar ms) =
  AUDistMatrixPerIndepVar
    [ (name, sliceAUDistMatrix testIdx trainIdx m) | (name,m) <- ms ]
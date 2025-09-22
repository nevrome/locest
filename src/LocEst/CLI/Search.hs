{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LocEst.CLI.Search where

import LocEst.Types
import           LocEst.Parsers
import           LocEst.CoreAlgorithms
import LocEst.Exceptions (throwL)
import LocEst.MathUtils
import           LocEst.Distance
import           LocEst.CLI.Utils

import qualified Data.Vector       as V
import qualified Data.Vector.Mutable   as VM
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable           as VUM
import           System.IO                     (hPutStrLn, stderr)

import           Statistics.Distribution           (logDensity, quantile)
import           Statistics.Distribution.StudentT  (StudentT)
import           Statistics.Distribution.Transform (LinearTransform)
import qualified Numeric.LinearAlgebra             as M
import qualified Data.Vector.Storable              as VS
import           Data.Conduit                  ((.|))
import qualified Data.Conduit                  as Con
import qualified Data.Conduit.Algorithms.Async as ConAA
import qualified Data.Conduit.Combinators      as ConC
import qualified Data.Conduit.List             as ConL
import qualified Data.Csv as Csv
import GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as Bchs
import Conduit (liftIO)
import Data.Maybe (mapMaybe)

data SearchOptions = SearchOptions
    { _searchInObservationFile   :: FilePath
    , _searchInIndepPredGridFile :: FilePath
    , _searchInTempGrid          :: Maybe [AbsRelTempPos]
    , _searchInDepSearchGrid     :: Maybe DepVarsPredGridSettings
    , _searchAlgorithm           :: KernelDefinition
    , _searchOutFile             :: Maybe FilePath
    }

runSearch :: SearchOptions -> Double -> IO ()
runSearch (SearchOptions
    inObsFile inIndepVarsPredGridFile maybeTempGrid inMaybeDepSearchGrid kernelDefinition outFile
    ) spatDistUnitScaling = do
    -- list of variables
    let depVars   = getKeys kernelDefinition
        indepVars = getKeys $ _kodvLengths $ head $ _kdefPerDepVar kernelDefinition
        kernels   = getValues kernelDefinition
    -- read observations
    !obs <- filterVarsInObs depVars indepVars <$> readObservations inObsFile
    -- read indepVar prediction grid positions
    !indepPredGrid <- V.map (filterVarsInIndepVarsPos indepVars) <$> readIndepVarsPos inIndepVarsPredGridFile
    -- read depVar search grid
    !depSearchGrid <- traverse (readDepVarsPredGrid depVars indepVars) inMaybeDepSearchGrid
    -- permutations
    hPutStrLn stderr "Preparing permutations"
    let permutations = createPermutations obs Nothing indepPredGrid depSearchGrid maybeTempGrid
    -- run interpolation and search
    hPutStrLn stderr "Running interpolation"
    Con.runConduitRes $
           ConC.yieldMany permutations
        .| ConL.concatMapM (liftIO . core spatDistUnitScaling depVars kernels)
        .| progress 1000 Nothing
        -- .| normalise normalisation
        .| sinkNamedCSV outFile
    putStrLn "Done"

core :: Double -> [DepVarName] -> [KernelOneDepVar] -> Permutation -> IO [SearchResultRow]
core spatDistUnitScaling depVars kernelsPerDepVar perm@(Permutation tempSamplingIteration obs grid searchDepVarPos) = do
    dists <- calcObsGridDistances spatDistUnitScaling obs grid
    -- TODO: case maybeDistFile of ...
    -- ... 
    let perDepVar = zipWith (interpol obs dists searchDepVarPos) depVars kernelsPerDepVar
        nGrid = V.length grid
        -- turn SSL to SSR
        rowsForGridIdx :: Int -> [SearchResultRow]
        rowsForGridIdx i =
            let resAtI = map (V.! i) perDepVar
                mkRow :: Maybe DepVarsPredPos -> [Maybe Double] -> SearchResultRow
                mkRow mSearchOne llsOne = SSRKAS
                    { _ssrKASTempSampIter     = tempSamplingIteration
                    , _ssrKASIndepVarsPos     = grid V.! i
                    , _ssrKASDepVarName       = map _sslKASDepVarName       resAtI
                    , _ssrKASEffN             = map _sslKASEffN             resAtI
                    , _ssrKASWeightedVar      = map _sslKASWeightedVar      resAtI
                    , _ssrKASWeightedVarPrior = map _sslKASWeightedVarPrior resAtI
                    , _ssrKASPosterior        = map _sslKASPosterior        resAtI
                    , _ssrKASLowerBound       = map _sslKASLowerBound       resAtI
                    , _ssrKASMedian           = map _sslKASMedian           resAtI
                    , _ssrKASUpperBound       = map _sslKASUpperBound       resAtI
                    , _ssrKASSearchPos        = mSearchOne
                    , _ssrKASLogLikelihood    = llsOne
                    , _ssrKASAggLogLikelihood = sumIfAllJust llsOne
                    }
                -- extract per-depVar likelihoods at index j (safely)
                llsAt :: Int -> [Maybe Double]
                llsAt j = [ mv >>= (V.!? j) | mv <- map _sslKASLogLikelihood resAtI ]
                -- branches for absent/present search candidates
                depCount  = length perDepVar
                rowsNoSearch :: [SearchResultRow]
                rowsNoSearch = [ mkRow Nothing (replicate depCount Nothing) ]
                rowsWithSearch :: V.Vector DepVarsPredPos -> [SearchResultRow]
                rowsWithSearch svec = V.toList $ V.imap (\j sp -> mkRow (Just sp) (llsAt j)) svec
            in maybe rowsNoSearch rowsWithSearch searchDepVarPos
    pure $ concatMap rowsForGridIdx [0 .. nGrid-1]

sumIfAllJust :: [Maybe Double] -> Maybe Double
sumIfAllJust xs = do
  ys <- sequence xs
  if null ys then Nothing else Just (sum ys)

zipWithN :: ([a] -> b) -> [V.Vector a] -> V.Vector b
zipWithN f vs 
  | null vs   = V.empty
  | otherwise = V.generate (V.length $ head vs) (\i -> f (map (V.! i) vs))


-- Permutation mechanism
data Permutation = Permutation {
      _permTempSamplingIteration :: Int
    , _permObs                   :: V.Vector Observation
    , _permIndepPredGrid         :: V.Vector IndepVarsPos
    , _permDepSearchGrid         :: Maybe (V.Vector DepVarsPredPos)
} deriving (Show)

createPermutations :: V.Vector Observation -> Maybe TempSampleMatrix
                    -> V.Vector IndepVarsPos -> Maybe (V.Vector DepVarsPredPos) -> Maybe [AbsRelTempPos]
                    -> [Permutation]
createPermutations obs maybeTempSampleMatrix indepPredGrid depSearchGrid maybeTempGrid = do
    -- apply temp resampling to obs
    tempSamp <- [0..(nrTempSamples maybeTempSampleMatrix - 1)]
    let modObs = V.map (applyTempSamp maybeTempSampleMatrix tempSamp) obs
    -- grid construction including time
    (indepPredPos, depSearchPos) <- splitDataByTempGrid maybeTempGrid indepPredGrid depSearchGrid
    return $ Permutation tempSamp modObs indepPredPos depSearchPos

nrTempSamples :: Maybe TempSampleMatrix -> Int
nrTempSamples Nothing                         = 1
nrTempSamples (Just (TempSampleMatrix n _ _)) = n

applyTempSamp :: Maybe TempSampleMatrix -> Int -> Observation -> Observation
applyTempSamp (Just m) i obs@(Observation i1 i2 (HyperPos (IndepSpatTempPos (SpatTempPos i3 (TempPos age))) i4) i5) =
    let obsIndex = getIndex obs
        newage = lookUpTempSample m i obsIndex
    in Observation i1 i2 (HyperPos (IndepSpatTempPos (SpatTempPos i3 (TempPos newage))) i4) i5
applyTempSamp _ _ obs = obs

splitDataByTempGrid :: Maybe [AbsRelTempPos] -> V.Vector IndepVarsPos -> Maybe (V.Vector DepVarsPredPos)
                    -> [(V.Vector IndepVarsPos, Maybe (V.Vector DepVarsPredPos))]
splitDataByTempGrid Nothing indepPredGrid depSearchGrid = [(indepPredGrid, depSearchGrid)]
splitDataByTempGrid (Just absRelTempPos) indepPredGrid depSearchGrid =
    let spatPos = V.mapMaybe (\(IndepSpatTempPos (SpatTempPos s _)) -> Just s) indepPredGrid
    in concat $ mapMaybe (\t -> build t spatPos depSearchGrid) absRelTempPos
  where
    build :: AbsRelTempPos -> V.Vector SpatPos -> Maybe (V.Vector DepVarsPredPos)
          -> Maybe [(V.Vector IndepVarsPos, Maybe (V.Vector DepVarsPredPos))]
    build (AbsTempPos yearBCAD) spatPos depGrid =
      let indepVarsPos = V.map (\s -> IndepSpatTempPos (SpatTempPos s (TempPos yearBCAD))) spatPos
      in Just [(indepVarsPos, depGrid)]
    build (RelTempPos yearDist) spatPos depGrid@(Just searchGrid) =
      let refAge = V.toList $ V.mapMaybe getObsAge searchGrid
          indepVarsPos = for refAge $
              \r -> V.map (\s -> IndepSpatTempPos (SpatTempPos s (TempPos $ r + yearDist))) spatPos
      in Just $ zip indepVarsPos (map Just $ V.group searchGrid)
    build _ _ _ = Nothing 
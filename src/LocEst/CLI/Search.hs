{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LocEst.CLI.Search where

import LocEst.Types
import LocEst.TypesFlat
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
import Data.Maybe (mapMaybe, isJust)
import Control.Concurrent.Async (async, wait)
import qualified Data.Map.Strict as Map
import Data.Foldable (foldl')

data SearchOptions = SearchOptions
    { _searchInObservationFile   :: FilePath
    , _searchInIndepPredGridFile :: FilePath
    , _searchInTempGrid          :: Maybe [AbsRelTempPos]
    , _searchInDepSearchGrid     :: Maybe DepVarsPredGridSettings
    , _searchAlgorithm           :: KernelDefinition
    , _searchInObsGridDistFile   :: Maybe FilePath
    , _searchOutFile             :: Maybe FilePath
    }

runSearch :: SearchOptions -> Double -> IO ()
runSearch (SearchOptions
    inObsFile inIndepVarsPredGridFile maybeTempGrid
    inMaybeDepSearchGrid kernelDefinition
    maybeObsGridDistFile
    outFile
    ) spatDistUnitScaling = do
    let algorithm = _kdefAlgorithm kernelDefinition
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
    -- read distances
    !obsGridDistances <- traverse (readAUDistMulti obs indepPredGrid) maybeObsGridDistFile
    -- permutations
    hPutStrLn stderr "Preparing permutations"
    let permutations = createPermutations obs Nothing indepPredGrid depSearchGrid maybeTempGrid
    -- run interpolation and search
    hPutStrLn stderr "Running interpolation"
    Con.runConduitRes $
           ConC.yieldMany permutations
        .| ConL.concatMapM (liftIO . core algorithm indepVars obsGridDistances spatDistUnitScaling depVars kernels)
        .| progress 1000 Nothing
        .| sinkNamedCSV outFile
    putStrLn "Done"

core :: Algorithm -> [IndepVarName] -> Maybe AUDistMatrixPerIndepVar -> Double
     -> [DepVarName] -> [KernelOneDepVar] -> Permutation
     -> IO [SearchResultRow]
core algorithm indepVars maybeObsGridDists spatDistUnitScaling
     depVars kernelsPerDepVar perm@(Permutation tempSamplingIteration obs grid searchDepVarPos) = do
    perDepVar <- case algorithm of
        GPR -> do
            -- gpr
            distsObsGrid <- case maybeObsGridDists of -- this could be refactored to be shorter
                Nothing -> do
                     aObsGrid  <- async $ auMatrixToFlat <$> calcObsGridDistances spatDistUnitScaling obs grid indepVars
                     wait aObsGrid
                Just (AUDistMatrixPerIndepVar ms) -> auMatrixToFlat . AUDistMatrixPerIndepVar <$>
                    forM indepVars (\name -> case lookup name ms of
                       Just m  -> pure (name, m)
                       Nothing -> calcObsGridOneDim spatDistUnitScaling obs grid name)
           
            aObsObs   <- async $ suMatrixToFlatHalf <$> calcObsObsDistances spatDistUnitScaling obs
            aGridGrid <- async $ suMatrixToFlatHalf <$> calcGridGridDistances spatDistUnitScaling grid
            distsObsObs   <- wait aObsObs
            distsGridGrid <- wait aGridGrid
            return $ zipWith (gpr obs grid distsObsGrid distsObsObs distsGridGrid searchDepVarPos) depVars kernelsPerDepVar
        KAS -> do
            -- kas
            distsObsGrid  <- auMatrixToFlat <$> calcObsGridDistances spatDistUnitScaling obs grid indepVars
            return $ zipWith (kas obs distsObsGrid searchDepVarPos) depVars kernelsPerDepVar
    -- turn SSL to SSR
    let rawRows = concatMap (rowsForGridIdx perDepVar) [0 .. (V.length grid)-1]
    if isJust searchDepVarPos && isSpatioTemporal grid
    then return $ normaliseByTimeSlice rawRows
    else return rawRows
    where
        rowsForGridIdx :: [V.Vector SearchResultLong] -> Int -> [SearchResultRow]
        rowsForGridIdx perDepVar i =
            let resAtI = map (V.! i) perDepVar
                mkRow :: Maybe DepVarsPredPos -> [Maybe Double] -> SearchResultRow
                mkRow mSearchOne llsOne = SSR
                    { _ssrTempSampIter     = tempSamplingIteration
                    , _ssrIndepVarsPos     = grid V.! i
                    , _ssrDepVarName       = map _sslDepVarName resAtI
                    , _ssrLowerBound       = map _sslLowerBound resAtI
                    , _ssrMedian           = map _sslMedian     resAtI
                    , _ssrUpperBound       = map _sslUpperBound resAtI
                    , _ssrSearchPos        = mSearchOne
                    , _ssrLogLikelihood    = llsOne
                    , _ssrAggLogLikelihood = sumIfAllJust llsOne
                    , _ssrProbability      = Nothing
                    }
                -- extract per-depVar likelihoods at index j (safely)
                llsAt :: Int -> [Maybe Double]
                llsAt j = [ mv >>= (V.!? j) | mv <- map _sslLogLikelihood resAtI ]
                -- branches for absent/present search candidates
                depCount  = length perDepVar
                rowsNoSearch :: [SearchResultRow]
                rowsNoSearch = [ mkRow Nothing (replicate depCount Nothing) ]
                rowsWithSearch :: V.Vector DepVarsPredPos -> [SearchResultRow]
                rowsWithSearch svec = V.toList $ V.imap (\j sp -> mkRow (Just sp) (llsAt j)) svec
            in maybe rowsNoSearch rowsWithSearch searchDepVarPos
        sumIfAllJust :: [Maybe Double] -> Maybe Double
        sumIfAllJust xs = do
          ys <- sequence xs
          if null ys then Nothing else Just (sum ys)

-- normalisation mechanism
normaliseByTimeSlice :: [SearchResultRow] -> [SearchResultRow]
normaliseByTimeSlice rows =
    -- group all log-likelihoods per time slice
    let grouped = foldl' (\m row ->
                      case _ssrAggLogLikelihood row of
                        Just ll -> Map.insertWith (++) (makeKey row) [ll] m
                        Nothing -> Map.insertWith (++) (makeKey row) [] m
                   ) Map.empty rows
    -- compute log–sum–exp denom per time slice
        factors = Map.map (\logs ->
                     let maxLog = if null logs then 0 else maximum logs
                         denom  = sum [exp (l - maxLog) | l <- logs]
                     in (maxLog, denom)
                  ) grouped
    -- normalise each row within its time slice
        normRow row = case (_ssrAggLogLikelihood row, Map.lookup (makeKey row) factors) of
            (Just ll, Just (maxLog, denom)) | denom > 0 ->
                 row { _ssrProbability = Just $ exp (ll - maxLog) / denom }
            _ -> row { _ssrProbability = Nothing }
    in map normRow rows

makeKey :: SearchResultRow -> (DepVarsPredPos, Int)
makeKey row =
    let searchPos = case _ssrSearchPos row of
            Just x -> x
            _ -> error "impossible state"
        t = case _ssrIndepVarsPos row of
            IndepSpatTempPos (SpatTempPos _ (TempPos x)) -> x
            _ -> error "impossible state"
    in (searchPos, t)

isSpatioTemporal v = case v V.!? 0 of
    Just (IndepSpatTempPos _) -> True
    _                         -> False

-- permutation mechanism
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
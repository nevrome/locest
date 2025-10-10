{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module LocEst.Distance where

import           LocEst.Exceptions
import           LocEst.Types
import           LocEst.TypesFlat

import qualified Data.Vector       as V
import qualified Data.Vector.Mutable   as VM
import qualified Data.Vector.Storable           as VS
import qualified Data.Vector.Storable.Mutable           as VSM
import           Control.Monad                 (replicateM, zipWithM_)
import Control.Applicative ((<|>))
import Data.Foldable (forM_)
import qualified Control.Monad as OP
import Data.Traversable (forM)

calcObsGridDistances :: Double -> V.Vector Observation -> V.Vector IndepVarsPos -> [IndepVarName] -> IO AUDistMatrixPerIndepVar
calcObsGridDistances spatScale obs grid varsToCompute = do
    let indepVarNames = case grid V.! 0 of
          IndepSpatTempPos _ -> ["space", "time"]
          IndepArbitraryDimPos (ValuesPerIndepVar ns _) -> V.toList ns
    let selected = filter (`elem` varsToCompute) indepVarNames
    mats <- mapM (calcObsGridOneDim spatScale obs grid) selected
    pure (AUDistMatrixPerIndepVar mats)

calcObsGridOneDim :: Double -> V.Vector Observation -> V.Vector IndepVarsPos -> IndepVarName -> IO (IndepVarName, AUDistMatrix)
calcObsGridOneDim spatScale obs grid varName = do
  case grid V.! 0 of
    IndepSpatTempPos _ ->
      case varName of
        "space" -> fmap (varName,) (computeSpaceAUDistMatrix spatScale obs grid)
        "time"  -> fmap (varName,) (computeTimeAUDistMatrix obs grid)
        _       -> error ("Unknown ST variable: " ++ varName)
    IndepArbitraryDimPos (ValuesPerIndepVar names _) ->
      case V.elemIndex varName names of
        Just ix -> fmap (varName,) (computeArbitraryAUDistMatrix ix obs grid)
        Nothing -> error ("Unknown arbitrary variable: " ++ varName)

computeSpaceAUDistMatrix :: Double -> V.Vector Observation -> V.Vector IndepVarsPos -> IO AUDistMatrix
computeSpaceAUDistMatrix spatScale obs grid = do
  let nrObs  = V.length obs
      nrGrid = V.length grid
  mv <- VSM.new (nrGrid * nrObs)
  forM_ [0 .. nrGrid-1] $ \gy ->
    let IndepSpatTempPos (SpatTempPos s2 _) = grid V.! gy
    in forM_ [0 .. nrObs-1] $ \ox ->
         let IndepSpatTempPos (SpatTempPos s1 _) = posFromObs (obs V.! ox)
         in VSM.write mv (gy*nrObs + ox) (spatialDistSpatPos s1 s2 * spatScale)
  frozen <- VS.unsafeFreeze mv
  pure (AUDistMatrix nrObs nrGrid frozen)

computeTimeAUDistMatrix :: V.Vector Observation -> V.Vector IndepVarsPos -> IO AUDistMatrix
computeTimeAUDistMatrix obs grid = do
  let nrObs  = V.length obs
      nrGrid = V.length grid
  mv <- VSM.new (nrGrid * nrObs)
  forM_ [0 .. nrGrid-1] $ \gy ->
    let IndepSpatTempPos (SpatTempPos _ t2) = grid V.! gy
    in forM_ [0 .. nrObs-1] $ \ox ->
         let IndepSpatTempPos (SpatTempPos _ t1) = posFromObs (obs V.! ox)
         in VSM.write mv (gy*nrObs + ox) (temporalDistTempPos t1 t2)
  frozen <- VS.unsafeFreeze mv
  pure (AUDistMatrix nrObs nrGrid frozen)

computeArbitraryAUDistMatrix :: Int -> V.Vector Observation -> V.Vector IndepVarsPos -> IO AUDistMatrix
computeArbitraryAUDistMatrix ix obs grid = do
  let nrObs  = V.length obs
      nrGrid = V.length grid
  mv <- VSM.new (nrGrid * nrObs)
  forM_ [0 .. nrGrid-1] $ \gy ->
    let IndepArbitraryDimPos (ValuesPerIndepVar _ vs2) = grid V.! gy
    in forM_ [0 .. nrObs-1] $ \ox ->
         let IndepArbitraryDimPos (ValuesPerIndepVar _ vs1) = posFromObs (obs V.! ox)
         in VSM.write mv (gy*nrObs + ox) (abs (vs1 VS.! ix - vs2 VS.! ix))
  frozen <- VS.unsafeFreeze mv
  pure (AUDistMatrix nrObs nrGrid frozen)

posFromObs (Observation _ _ (HyperPos ivpos _) _) = ivpos

auMatrixToFlat :: AUDistMatrixPerIndepVar -> IndepVarsDistFlat
auMatrixToFlat audmPerIndepVar =
    let mats = getValues audmPerIndepVar -- [AUDistMatrix]
        stride = length mats             -- number of dimensions
        nRows  = _audmNrRows (head mats) -- grid size
        nCols  = _audmNrCols (head mats) -- obs size
        total  = nRows * nCols
        -- tags vector: assume all same type — infer from dimension names or external info
        tagsVec = VS.replicate total $
                    case head (getKeys audmPerIndepVar) of
                      "space" -> False
                      _       -> True
        -- payload: interleave dimensions per row/col
        payloadVec = VS.generate (total * stride) $ \k ->
            let (idx, dimIx) = k `divMod` stride
            in _audmMatrix (mats !! dimIx) VS.! idx
    in IndepVarsDistFlat tagsVec payloadVec stride

calcObsObsDistances :: Double -> V.Vector Observation -> IO SUDistMatrixPerIndepVar
calcObsObsDistances scale = calcDistancesHalf scale (\(Observation _ _ (HyperPos pos _) _) -> pos)

calcGridGridDistances :: Double -> V.Vector IndepVarsPos -> IO SUDistMatrixPerIndepVar
calcGridGridDistances scale = calcDistancesHalf scale id

-- optimised symmetric distance calculation
calcDistancesHalf :: Double -> (a -> IndepVarsPos) -> V.Vector a -> IO SUDistMatrixPerIndepVar
calcDistancesHalf spatScale getPos vec = do
    let n = V.length vec
        indepVarNames = case getPos (V.head vec) of
          IndepSpatTempPos _ -> ["space", "time"]
          IndepArbitraryDimPos (ValuesPerIndepVar ns _) -> V.toList ns
        nHalf = n * (n+1) `div` 2
    -- mutable vectors for each indep var (half matrix length)
    matsMV <- forM indepVarNames (\_ -> VSM.new nHalf)
    -- fill each dimension's half matrix (triangle including diagonal)
    let idxHalf i j = i * (i+1) `div` 2 + j
    forM_ [0 .. n-1] $ \i -> do
      let pi = getPos (vec V.! i)
      forM_ [0 .. i] $ \j -> do
        let pj  = getPos (vec V.! j)
            idx = idxHalf i j
        case (pi, pj) of
          (IndepSpatTempPos (SpatTempPos s1 t1),
           IndepSpatTempPos (SpatTempPos s2 t2)) -> do
              -- dim 0: space
              let sd = spatialDistSpatPos s1 s2 * spatScale
              VSM.write (matsMV !! 0) idx sd
              -- dim 1: time
              let td = temporalDistTempPos t1 t2
              VSM.write (matsMV !! 1) idx td
          (IndepArbitraryDimPos (ValuesPerIndepVar _ vs1),
           IndepArbitraryDimPos (ValuesPerIndepVar _ vs2)) -> do
              let dvec = allDistancesVS vs1 vs2
              forM_ [0 .. VS.length dvec - 1] $ \dimIx ->
                VSM.write (matsMV !! dimIx) idx (dvec VS.! dimIx)
          _ -> throwL "Mismatch in independent variable definitions"
    -- freeze and build SUDistMatrixPerIndepVar
    frozen <- forM (zip indepVarNames matsMV) $ \(name, mv) -> do
                 v <- VS.unsafeFreeze mv
                 pure (name, SUDistMatrix v)
    pure $ SUDistMatrixPerIndepVar frozen

suMatrixToFlatHalf :: SUDistMatrixPerIndepVar -> IndepVarsDistFlat
suMatrixToFlatHalf sudmPerIndepVar =
    let mats   = getValues sudmPerIndepVar -- [SUDistMatrix]
        stride = length mats               -- number of dimensions
        nHalf  = VS.length (let (SUDistMatrix v) = head mats in v)
        -- tags vector: same simple heuristic as auMatrixToFlat
        tagsVec = VS.replicate nHalf $
                    case head (getKeys sudmPerIndepVar) of
                      "space" -> False
                      _       -> True
        -- payload: stride‐interleave each dim's half vector
        payloadVec = VS.generate (nHalf * stride) $ \k ->
            let (idx, dimIx) = k `divMod` stride
                SUDistMatrix vec = mats !! dimIx
            in vec VS.! idx
    in IndepVarsDistFlat tagsVec payloadVec stride

-- distance helper functions

{-# INLINE allDistancesVS #-}
allDistancesVS :: VS.Vector Double -> VS.Vector Double -> VS.Vector Double
allDistancesVS = VS.zipWith (\x y -> abs (x - y))

allDistances :: [Double] -> [Double] -> [Double]
allDistances = zipWith (\x y -> abs (x - y))

euclideanDistance :: [Double] -> [Double] -> Double
euclideanDistance list1 list2 =
  let squaredDifferences = zipWith (\x y -> (x - y) ** 2) list1 list2
  in sqrt $ sum squaredDifferences

{-# INLINE temporalDistTempPos #-}
temporalDistTempPos :: TempPos -> TempPos -> Double
temporalDistTempPos (TempPos t1) (TempPos t2) = temporalDistYearBCAD t1 t2

temporalDistYearBCAD :: YearBCAD -> YearBCAD -> Double
temporalDistYearBCAD t1 t2 = fromIntegral $ abs (t1 - t2)

spatialDistSpatTempPos :: SpatTempPos -> SpatTempPos -> Double
spatialDistSpatTempPos (SpatTempPos spatP1 _) (SpatTempPos spatP2 _) =
    spatialDistSpatPos spatP1 spatP2

{-# INLINE spatialDistSpatPos #-}
spatialDistSpatPos :: SpatPos -> SpatPos -> Double
spatialDistSpatPos (SpatPosCartesian p1) (SpatPosCartesian p2) = spatialDistCartesianPos p1 p2
spatialDistSpatPos (SpatPosLongLat p1) (SpatPosLongLat p2) = spatialDistLongLatPos p1 p2
spatialDistSpatPos _ _ = error "Can not be calculated"

spatialDistCartesianPos :: CartesianPos -> CartesianPos -> Double
spatialDistCartesianPos (CartesianPos _ _ x1 y1) (CartesianPos _ _ x2 y2) =
    sqrt (((x1 - x2) ** 2) + ((y1 - y2) ** 2))

-- Haversine distance in metres
spatialDistLongLatPos :: LongLatPos -> LongLatPos -> Double
spatialDistLongLatPos (LongLatPos _ _ (Longitude lon1) (Latitude lat1))
                      (LongLatPos _ _ (Longitude lon2) (Latitude lat2)) =
    let r = 6371000  -- radius of Earth in metres
        toRadians n = n * pi / 180
        square x = x * x
        cosr = cos . toRadians
        dlat = toRadians (lat1 - lat2) / 2
        dlon = toRadians (lon1 - lon2) / 2
        a = square (sin dlat) + cosr lat1 * cosr lat2 * square (sin dlon)
        c = 2 * atan2 (sqrt a) (sqrt (1 - a))
    in r * c

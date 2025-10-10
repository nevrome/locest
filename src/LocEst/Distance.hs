{-# LANGUAGE BangPatterns #-}

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

-- calcObsGridDistances :: Double -> V.Vector Observation -> V.Vector IndepVarsPos -> IO IndepVarsDistFlat
-- calcObsGridDistances scale obs grid =
--     commonDistances
--         scale (V.length grid) (V.length obs)
--         id
--         (\(Observation _ _ (HyperPos pos _) _) -> pos)
--         grid obs

calcObsGridDistances :: Double -> V.Vector Observation -> V.Vector IndepVarsPos -> IO AUDistMatrixPerIndepVar
calcObsGridDistances spatScale obs grid = do
    let nrObs   = V.length obs
        nrGrid  = V.length grid
        indepVarNames = case grid V.! 0 of
          IndepSpatTempPos _ ->
              ["space", "time"]
          IndepArbitraryDimPos (ValuesPerIndepVar ns _) ->
              V.toList ns
    -- mutable vectors for each indep var (full rectangular m*n)
    matsMV <- forM indepVarNames (\_ -> VSM.new (nrGrid * nrObs))
    -- fill each dimension's matrix
    forM_ [0 .. nrGrid-1] $ \gy -> do
      let gpos = grid V.! gy
      forM_ [0 .. nrObs-1] $ \ox -> do
        let opos = obs V.! ox
            idx = gy * nrObs + ox
        case (posFromObs opos, gpos) of
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
          _ -> throwL "mismatch in indep variable definitions"
    -- freeze and build AUDistMatrixPerIndepVar
    frozen <- forM (zip indepVarNames matsMV) $
      \(name, mv) -> do
           v <- VS.unsafeFreeze mv
           pure (name, AUDistMatrix nrObs nrGrid v)
    pure $ AUDistMatrixPerIndepVar frozen
  where
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

calcObsObsDistancesFlat :: Double -> V.Vector Observation -> IO IndepVarsDistFlat
calcObsObsDistancesFlat scale = commonDistancesHalf scale (\(Observation _ _ (HyperPos pos _) _) -> pos)

calcGridGridDistancesFlat :: Double -> V.Vector IndepVarsPos -> IO IndepVarsDistFlat
calcGridGridDistancesFlat scale = commonDistancesHalf scale id

type GetPos a = a -> IndepVarsPos
commonDistances
   :: Double  -- scaling factor
  -> Int      -- number of rows
  -> Int      -- number of cols
  -> GetPos r -- extract IndepVarsPos from row element
  -> GetPos c -- extract IndepVarsPos from col element
  -> V.Vector r
  -> V.Vector c
  -> IO IndepVarsDistFlat
commonDistances spatDistUnitScaling nRows nCols getPosRow getPosCol rows cols = do
    let stride = case getPosCol (cols V.! 0) of
            IndepArbitraryDimPos (ValuesPerIndepVar ns _) -> V.length ns
            _                                             -> 2
    -- create empty result vectors
    tagsMV    <- VSM.new (nRows * nCols)
    payloadMV <- VSM.new (nRows * nCols * stride)
    -- distance calculation loop for all pairs
    forM_ [0 .. nRows-1] $ \i -> do
      let pi = getPosRow (rows V.! i)
      forM_ [0 .. nCols-1] $ \j -> do
        let pj  = getPosCol (cols V.! j)
            idx = i * nCols + j
        case (pi, pj) of
          (IndepSpatTempPos (SpatTempPos s1 t1),
           IndepSpatTempPos (SpatTempPos s2 t2)) -> do
              let !sd = spatialDistSpatPos s1 s2 * spatDistUnitScaling
                  !td = temporalDistTempPos t1 t2
              VSM.write tagsMV idx False
              VSM.write payloadMV (idx*stride)     sd
              VSM.write payloadMV (idx*stride + 1) td
          (IndepArbitraryDimPos (ValuesPerIndepVar _ vs1),
           IndepArbitraryDimPos (ValuesPerIndepVar _ vs2)) -> do
              let dvec = allDistancesVS vs1 vs2
              VSM.write tagsMV idx True
              VS.copy (VSM.slice (idx*stride) stride payloadMV) dvec
          _ -> throwL "mismatch in independent variable definitions"
    -- freeze result vectors
    tagsVS    <- VS.unsafeFreeze tagsMV
    payloadVS <- VS.unsafeFreeze payloadMV
    pure $ IndepVarsDistFlat tagsVS payloadVS stride

-- optimised symmetric distance calculation
commonDistancesHalf ::
     Double     -- ^ scaling factor
  -> GetPos a   -- ^ position extractor
  -> V.Vector a
  -> IO IndepVarsDistFlat
commonDistancesHalf spatDistUnitScaling getPos vec = do
    let n = V.length vec
        stride = case getPos (vec V.! 0) of
                   IndepArbitraryDimPos (ValuesPerIndepVar ns _) -> V.length ns
                   _                                             -> 2
        nHalf = n * (n+1) `div` 2 -- size of symmetrical half
    -- create empty result vectors
    tagsMV    <- VSM.new nHalf
    payloadMV <- VSM.new (nHalf * stride)
    -- walk triangle only
    let idxHalf i j = i * (i+1) `div` 2 + j
    forM_ [0 .. n-1] $ \i -> do
      let pi = getPos (vec V.! i)
      forM_ [0 .. i] $ \j -> do
        let pj  = getPos (vec V.! j)
            idx = idxHalf i j
        case (pi,pj) of
          (IndepSpatTempPos (SpatTempPos s1 t1),
           IndepSpatTempPos (SpatTempPos s2 t2)) -> do
              let !sd = spatialDistSpatPos s1 s2 * spatDistUnitScaling
                  !td = temporalDistTempPos t1 t2
              VSM.write tagsMV idx False
              VSM.write payloadMV (idx*stride)     sd
              VSM.write payloadMV (idx*stride+1)   td
          (IndepArbitraryDimPos (ValuesPerIndepVar _ vs1),
           IndepArbitraryDimPos (ValuesPerIndepVar _ vs2)) -> do
              let dvec = allDistancesVS vs1 vs2
              VSM.write tagsMV idx True
              VS.copy (VSM.slice (idx*stride) stride payloadMV) dvec
          _ -> throwL "Mismatch in independent variables"
    -- freeze result vectors
    tagsVS    <- VS.unsafeFreeze tagsMV
    payloadVS <- VS.unsafeFreeze payloadMV
    pure $ IndepVarsDistFlat tagsVS payloadVS stride

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

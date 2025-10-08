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

calcObsGridDistances :: Double -> V.Vector Observation -> V.Vector IndepVarsPos -> IO IndepVarsDistFlat
calcObsGridDistances scale obs grid =
    commonDistances
        scale (V.length grid) (V.length obs)
        id
        (\(Observation _ _ (HyperPos pos _) _) -> pos)
        grid obs

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

-- optimised symmetric distance calculation (upper triangle mirroring)
commonDistancesHalf ::
     Double     -- ^ scaling factor
  -> GetPos a   -- ^ position extractor
  -> V.Vector a
  -> IO IndepVarsDistFlat
commonDistancesHalf spatDistUnitScaling getPos vec = do
    let nRows = V.length vec
        nCols = nRows
        stride = case getPos (vec V.! 0) of
            IndepArbitraryDimPos (ValuesPerIndepVar ns _) -> V.length ns
            _                                             -> 2
    -- create empty result vectors
    tagsMV    <- VSM.new (nRows * nCols)
    payloadMV <- VSM.new (nRows * nCols * stride)
    -- loop upper triangle including diagonal
    forM_ [0 .. nRows-1] $ \i -> do
      let !pi = getPos (vec V.! i)
      forM_ [i .. nCols-1] $ \j -> do
        let !pj     = getPos (vec V.! j)
            !idx    = i * nCols + j
            !idxSym = j * nCols + i
        case (pi, pj) of
          (IndepSpatTempPos (SpatTempPos s1 t1),
           IndepSpatTempPos (SpatTempPos s2 t2)) -> do
              let !sd = spatialDistSpatPos s1 s2 * spatDistUnitScaling
                  !td = temporalDistTempPos t1 t2
              -- write (i,j)
              VSM.write tagsMV idx False
              VSM.write payloadMV (idx*stride)     sd
              VSM.write payloadMV (idx*stride + 1) td
              -- mirror (this also copies the diagonal, prevent with (i /= j))
              VSM.write tagsMV idxSym False
              VSM.write payloadMV (idxSym*stride)     sd
              VSM.write payloadMV (idxSym*stride + 1) td
          (IndepArbitraryDimPos (ValuesPerIndepVar _ vs1),
           IndepArbitraryDimPos (ValuesPerIndepVar _ vs2)) -> do
              let dvec = allDistancesVS vs1 vs2
              -- write (i,j)
              VSM.write tagsMV idx True
              VS.copy (VSM.slice (idx*stride) stride payloadMV) dvec
              -- mirror
              VSM.write tagsMV idxSym True
              VS.copy (VSM.slice (idxSym*stride) stride payloadMV) dvec
          _ -> throwL "mismatch in independent variable definitions"
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

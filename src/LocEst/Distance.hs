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


calcObsGridDistances :: Double -> V.Vector Observation -> V.Vector IndepVarsPos -> IO IndepVarsDistFlat
calcObsGridDistances spatDistUnitScaling obs grid = do
    let !nrObs = V.length obs
        !nrGrid = V.length grid
        !nrPairs = nrObs * nrGrid
    -- determine stride for arbitrary case:
    let stride = case grid V.!? 0 of
           Just (IndepArbitraryDimPos (ValuesPerIndepVar ns _)) -> V.length ns
           _                                                    -> 2
    -- create empty result vectors
    tagsMV    <- VSM.new nrPairs :: IO (VSM.IOVector Bool)
    payloadMV <- VSM.new (nrPairs * stride) :: IO (VSM.IOVector Double)
    -- nested loop to match all obs and grid positions
    forM_ [0 .. nrGrid-1] $ \gy -> do
      let gpos = grid `V.unsafeIndex` gy
      forM_ [0 .. nrObs-1] $ \ox -> do
        let !idx = gy * nrObs + ox
        let opos = obs `V.unsafeIndex` ox
        case (posFromObs opos, gpos) of
          (IndepSpatTempPos (SpatTempPos s1 t1),
           IndepSpatTempPos (SpatTempPos s2 t2)) -> do
              let !sd = spatialDistSpatPos s1 s2 * spatDistUnitScaling
                  !td = temporalDistTempPos t1 t2
              VSM.write tagsMV idx False
              VSM.write payloadMV (idx*stride)     sd
              VSM.write payloadMV (idx*stride + 1) td
          (IndepArbitraryDimPos (ValuesPerIndepVar _ vs1),
           IndepArbitraryDimPos (ValuesPerIndepVar _ vs2)) -> do
              let !dvec = allDistancesVS vs1 vs2
              VSM.write tagsMV idx True
              VS.copy (VSM.slice (idx*stride) stride payloadMV) dvec
          _ -> throwL "mismatch in independent variable definitions"
    -- freeze result vectors
    tagsVS    <- VS.unsafeFreeze tagsMV
    payloadVS <- VS.unsafeFreeze payloadMV
    pure $ IndepVarsDistFlat tagsVS payloadVS stride
  where
    isArb (IndepArbitraryDimPos _) = True
    isArb _                        = False
    posFromObs (Observation _ _ (HyperPos ivpos _) _) = ivpos

calcObsObsDistancesFlat :: Double -> V.Vector Observation -> IO IndepVarsDistFlat
calcObsObsDistancesFlat spatDistUnitScaling obs = do
    let !nrObs    = V.length obs
        !nrPairs  = nrObs * nrObs
        -- determine stride: number of independent vars in arbitrary case
        stride = case obs V.!? 0 of
            Just (Observation _ _ (HyperPos (IndepArbitraryDimPos (ValuesPerIndepVar ns _)) _) _) -> V.length ns
            _ -> 2
    -- create empty result vectors
    tagsMV    <- VSM.new nrPairs        :: IO (VSM.IOVector Bool)
    payloadMV <- VSM.new (nrPairs * stride) :: IO (VSM.IOVector Double)
    -- loop over all pairs (i,j)
    forM_ [0 .. nrObs-1] $ \i -> do
      let oi = obs `V.unsafeIndex` i
      forM_ [0 .. nrObs-1] $ \j -> do
        let oj  = obs `V.unsafeIndex` j
            idx = i * nrObs + j  -- full matrix row-major index
        case (posFromObs oi, posFromObs oj) of
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
  where
    posFromObs (Observation _ _ (HyperPos ivpos _) _) = ivpos

calcGridGridDistancesFlat :: Double -> V.Vector IndepVarsPos -> IO IndepVarsDistFlat
calcGridGridDistancesFlat spatDistUnitScaling grid = do
    let !nrGrid  = V.length grid
        !nrPairs = nrGrid * nrGrid
        -- determine stride
        stride = case grid V.!? 0 of
            Just (IndepArbitraryDimPos (ValuesPerIndepVar ns _)) -> V.length ns
            _                                                    -> 2
    -- create empty result vectors
    tagsMV    <- VSM.new nrPairs                :: IO (VSM.IOVector Bool)
    payloadMV <- VSM.new (nrPairs * stride)     :: IO (VSM.IOVector Double)
    -- loop over all (i, j)
    forM_ [0 .. nrGrid-1] $ \i -> do
      let gi = grid `V.unsafeIndex` i
      forM_ [0 .. nrGrid-1] $ \j -> do
        let gj  = grid `V.unsafeIndex` j
            idx = i * nrGrid + j
        case (gi, gj) of
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
    -- freeze
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

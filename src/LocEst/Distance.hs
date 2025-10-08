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

makeObsPairs :: V.Vector Observation -> [(Int, (Observation, Observation))]
makeObsPairs obs =
    let obsIndexMax = V.length obs - 1
        obsPairs = [(obs V.! x, obs V.! y) | x <- [0..obsIndexMax], y <- [0..obsIndexMax], x > y]
    in zip [0..] obsPairs

calcObsDistances :: Double -> V.Vector Observation -> IO MatrixPerIndepVar
calcObsDistances spatDistUnitScaling obs = do
    let obsPairs = makeObsPairs obs
        nrPairs = length obsPairs
        (Observation _ _ (HyperPos indepPos _) _) = V.head obs
    case indepPos of
        -- spatiotemporal system
        (IndepSpatTempPos _) -> do
            -- create mutable vectors to write distances directly
            spaceVec <- VSM.new nrPairs
            timeVec  <- VSM.new nrPairs
            -- calculate and write distances to mutable memory
            mapM_ (distSpaceTime spaceVec timeVec) obsPairs
            -- make result vectors immutable for easier handling
            spaceVecNonMut <- VS.unsafeFreeze spaceVec
            timeVecNonMut  <- VS.unsafeFreeze timeVec
            return $ MatrixPerIndepVar [("space", SUDistMatrix spaceVecNonMut), ("time", SUDistMatrix timeVecNonMut)]
        -- arbitrary dimension system
        (IndepArbitraryDimPos pos@(ValuesPerIndepVar ns vs)) -> do
            arbitraryVecs <- replicateM (length ns) (VSM.new nrPairs)
            mapM_ (distArbitrary arbitraryVecs) obsPairs
            arbitraryVecsNonMut <- mapM VS.unsafeFreeze arbitraryVecs
            return $ MatrixPerIndepVar $ zipWith (\name vec -> (name, SUDistMatrix vec)) (getKeys pos) arbitraryVecsNonMut
    where
        distSpaceTime :: VSM.IOVector Double -> VSM.IOVector Double -> (Int, (Observation, Observation)) -> IO ()
        distSpaceTime
            spaceVec timeVec
            (i,
            (Observation i1 _ (HyperPos (IndepSpatTempPos (SpatTempPos s1 t1)) _) _,
             Observation i2 _ (HyperPos (IndepSpatTempPos (SpatTempPos s2 t2)) _) _)
            ) = do
            let timeDist  = temporalDistTempPos t1 t2
                spaceDist = spatialDistSpatPos s1 s2
                spaceDistScaled = spaceDist * spatDistUnitScaling
            -- write distances to mutable vector
            VSM.write spaceVec i spaceDistScaled
            VSM.write timeVec  i timeDist
        distSpaceTime _ _ _ = error "impossible state in spatial independent variable distance calculation"
        distArbitrary :: [VSM.IOVector Double] -> (Int, (Observation, Observation)) -> IO ()
        distArbitrary
            arbitraryVecs
            (i,
            (Observation _ _ (HyperPos (IndepArbitraryDimPos (ValuesPerIndepVar _ vs1)) _) _,
             Observation _ _ (HyperPos (IndepArbitraryDimPos (ValuesPerIndepVar _ vs2)) _) _)
            ) = do
            -- this assumes that p1 and p2 have the same order of indep variables
            let arbitraryDists = allDistances (VS.toList vs1) (VS.toList vs2)
            zipWithM_ (`VSM.write` i) arbitraryVecs arbitraryDists
        distArbitrary _ _ = error "impossible state in arbitrary independent variable distance calculation"

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

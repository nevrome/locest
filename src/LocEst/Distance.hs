module LocEst.Distance where

import           LocEst.Types

allDistances :: [Double] -> [Double] -> [Double]
allDistances = zipWith (\x y -> abs (x - y))

euclideanDistance :: [Double] -> [Double] -> Double
euclideanDistance list1 list2 =
  let squaredDifferences = zipWith (\x y -> (x - y) ** 2) list1 list2
  in sqrt $ sum squaredDifferences

spatTempDistSpatTempPos :: SpatTempPos -> SpatTempPos -> SpatTempDist
spatTempDistSpatTempPos p1 p2 =
    SpatTempDist (spatialDistSpatTempPos p1 p2) (temporalDistSpatTempPos p1 p2)

temporalDistSpatTempPos :: SpatTempPos -> SpatTempPos -> Double
temporalDistSpatTempPos (SpatTempPos _ tempP1) (SpatTempPos _ tempP2) =
    temporalDistTempPos tempP1 tempP2

temporalDistTempPos :: TempPos -> TempPos -> Double
temporalDistTempPos (TempPos t1) (TempPos t2) = temporalDistYearBCAD t1 t2

temporalDistYearBCAD :: YearBCAD -> YearBCAD -> Double
temporalDistYearBCAD t1 t2 = fromIntegral $ abs (t1 - t2)

spatialDistSpatTempPos :: SpatTempPos -> SpatTempPos -> Double
spatialDistSpatTempPos (SpatTempPos spatP1 _) (SpatTempPos spatP2 _) =
    spatialDistSpatPos spatP1 spatP2

spatialDistSpatPos :: SpatPos -> SpatPos -> Double
spatialDistSpatPos (SpatPosCartesian p1) (SpatPosCartesian p2) = spatialDistCartesianPos p1 p2
spatialDistSpatPos (SpatPosLongLat p1) (SpatPosLongLat p2) = spatialDistLongLatPos p1 p2
spatialDistSpatPos _ _ = error "Can not be calculated"

spatialDistCartesianPos :: CartesianPos -> CartesianPos -> Double
spatialDistCartesianPos (CartesianPos _ _ x1 y1) (CartesianPos _ _ x2 y2) =
    sqrt (((x1 - x2)^(2 :: Int)) + ((y1 - y2)^(2 :: Int)))

-- Haversine distance
spatialDistLongLatPos :: LongLatPos -> LongLatPos -> Double
spatialDistLongLatPos (LongLatPos _ _ (Longitude lon1) (Latitude lat1))
                      (LongLatPos _ _ (Longitude lon2) (Latitude lat2)) =
    let r = 6371000  -- radius of Earth in meters
        toRadians n = n * pi / 180
        square x = x * x
        cosr = cos . toRadians
        dlat = toRadians (lat1 - lat2) / 2
        dlon = toRadians (lon1 - lon2) / 2
        a = square (sin dlat) + cosr lat1 * cosr lat2 * square (sin dlon)
        c = 2 * atan2 (sqrt a) (sqrt (1 - a))
    in (r * c) / 1000

module LocEst.Distance where

import           LocEst.Exceptions
import           LocEst.Types

import qualified Data.Vector       as V

filterObs ::
       Double
    -> Supplement
    -> Permutation
    -> V.Vector Observation
    -> (V.Vector Observation, V.Vector IndepVarsDist)
filterObs spatDistUnitScaling (Supplement filterThresholds maybeSpatDistMap maybeTempSamples) sett =
    V.unzip . V.mapMaybe handleOne
    where
        handleOne :: Observation -> Maybe (Observation, IndepVarsDist)
        handleOne obs =
            let dist = getDist spatDistUnitScaling maybeSpatDistMap maybeTempSamples sett obs
            in if inFilterRange filterThresholds dist
               then Just (obs,dist)
               else Nothing

getDist ::
       Double -> Maybe SpatDistMatrix -> Maybe TempSampleMatrix
    -> Permutation
    -> Observation
    -> IndepVarsDist
-- spatiotemporal distances
getDist
    spatDistUnitScaling maybeSpatDistMap maybeTempSamples
    (Permutation (IndepSpatTempPos (SpatTempPos gridSpatPos gridTempPos)) _ _ tempSampIteration _)
    (Observation obsIndex _ (HyperPos (IndepSpatTempPos (SpatTempPos obsSpatPos obsTempPos)) _) _) =
        let spatDist = findSpatDist maybeSpatDistMap
            spaceDistScaled = spatDist * spatDistUnitScaling
            tempDist = findTempDist maybeTempSamples
        in IndepSpatTempDist (SpatTempDist spaceDistScaled tempDist)
        where
            -- temporal distances
            findTempDist :: Maybe TempSampleMatrix -> Double
            -- calculate distances from mean ages
            findTempDist Nothing = temporalDistTempPos gridTempPos obsTempPos
            -- look up age samples and calculate distances from them
            findTempDist (Just tempSampleMatrix) =
                let (TempPos gridPointAge) = gridTempPos
                    obsAgeSample = lookUpTempSample tempSampleMatrix tempSampIteration obsIndex
                in temporalDistYearBCAD gridPointAge obsAgeSample
            -- spatial distances
            findSpatDist :: Maybe SpatDistMatrix -> Double
            -- calculate distances
            findSpatDist Nothing = spatialDistSpatPos gridSpatPos obsSpatPos
            -- look up distances
            findSpatDist (Just spatDistMatrix) =
                let gridSpatPosIndex = getIndex gridSpatPos
                in lookUpDistanceAU spatDistMatrix gridSpatPosIndex obsIndex
-- arbitrary dim distances
getDist
    _ _ _
    (Permutation (IndepArbitraryDimPos gridAbritryDimPos) _ _ _ _)
    (Observation _ _ (HyperPos (IndepArbitraryDimPos obsArbitraryDimPos) _) _) =
        let keys = getKeys obsArbitraryDimPos
            obsPos  = getValues obsArbitraryDimPos
            gridPos = getValues gridAbritryDimPos
            arbitraryDimDist = makeValuesPerIndepVar $ zip keys (allDistances obsPos gridPos)
        in IndepArbitraryDimDist arbitraryDimDist
-- wrong input
getDist _ _ _ _ _ = throwL "mismatch of independent variable definitions in distance calculation"

inFilterRange :: Maybe DistanceThresholds -> IndepVarsDist -> Bool
inFilterRange Nothing _ = True
inFilterRange
    (Just (SpaceTimeFilterThresholds minFilter maxFilter))
    (IndepSpatTempDist (SpatTempDist spatDistsKM tempDist)) =
    let minDecision = case minFilter of
            Nothing -> True
            Just (spaceMinFilter, timeMinFilter) -> spatDistsKM >= spaceMinFilter && tempDist >= timeMinFilter
        maxDecision = case maxFilter of
            Nothing -> True
            Just (spaceMaxFilter, timeMaxFilter) -> spatDistsKM <= spaceMaxFilter && tempDist <= timeMaxFilter
    in minDecision && maxDecision
inFilterRange
    (Just (ArbitraryDimFilterThresholds minFilter maxFilter))
    (IndepArbitraryDimDist (ValuesPerIndepVar dists)) =
    let minDecision = case minFilter of
            Nothing -> True
            Just (ValuesPerIndepVar minThresholds) -> all (\((_,x), (_,y)) -> x >= y) $ zip dists minThresholds
        maxDecision = case maxFilter of
            Nothing -> True
            Just (ValuesPerIndepVar maxThresholds) -> all (\((_,x), (_,y)) -> x <= y) $ zip dists maxThresholds
    in minDecision && maxDecision
inFilterRange _ _ = True

-- distance helper functions

allDistances :: [Double] -> [Double] -> [Double]
allDistances = zipWith (\x y -> abs (x - y))

euclideanDistance :: [Double] -> [Double] -> Double
euclideanDistance list1 list2 =
  let squaredDifferences = zipWith (\x y -> (x - y) ** 2) list1 list2
  in sqrt $ sum squaredDifferences

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

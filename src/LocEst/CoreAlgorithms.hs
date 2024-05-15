module LocEst.CoreAlgorithms where

import           LocEst.Distance
import           LocEst.MathUtils
import           LocEst.Types
import           LocEst.Utils

import qualified Control.Monad.Except    as E
import           Data.Maybe              (mapMaybe)
import           Statistics.Distribution (density, quantile)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

type CoreLog = E.Except LOCESTException

data CoreOutMode =
      CoreOutShort
    | CoreOutFull
    | CoreOutObsWeight Int

core :: CoreOutMode -> CoreSupplement -> V.Vector Observation -> CorePermutation -> CoreLog CoreOut
core (CoreOutObsWeight nrTopObs) supp observations sett@(CorePermutation _ _ kernelDefinition _) = do
    let namePerDepVar  = getKeys kernelDefinition
    obsWithWeights <- V.mapMaybeM (getWeightsPerObs supp sett namePerDepVar) observations
    return $ CoreObsWeight (V.map (ObsWeight sett) obsWithWeights)
core outMode supp observations sett@(CorePermutation _ searchDepVarPos kernelDefinition _) = do
    let namePerDepVar  = getKeys kernelDefinition
    obsWithWeights <- V.mapMaybeM (getWeightsPerObs supp sett namePerDepVar) observations
    let valuePerDepVar = case searchDepVarPos of
            Just x  -> Just <$> getValues x
            Nothing -> replicate (length namePerDepVar) Nothing
    interpolPerDepVarFull <- mapM (interpolAndSearchOneDepVar obsWithWeights) $ zip namePerDepVar valuePerDepVar
    let interpolPerDepVar = case outMode of
            CoreOutShort -> map resOneDepvar2Short interpolPerDepVarFull
            CoreOutFull -> interpolPerDepVarFull
    -- compile output object
    return $ CoreSearchResult $ SearchResult {
           _srCorePermutation = sett
         , _srInterpolation   = InterpolationResult interpolPerDepVar
         , _srProbability     = case mapMaybe getProbability interpolPerDepVarFull of
            [] -> Nothing
            xs -> Just $ foldSum xs -- TODO: Should probably be a product
         }

getWeightsPerObs :: CoreSupplement -> CorePermutation -> [DepVarName] -> Observation -> CoreLog (Maybe ObsWithWeights)
-- spatiotemporal distances
getWeightsPerObs
    (CoreSupplement maybeSpaceTimeFilter maybeSpatDistMap maybeTempSamples)
    (CorePermutation (IndepSpatTempPos gridSpatTempPos) _ kernelDefinition tempSampIteration)
    depVars
    obs@(Observation obsIndex _ (HyperPos (IndepSpatTempPos obsSpatTempPos) _)) =
        -- get dists
        let tempDist = findTempDist maybeTempSamples
            spatDist = findSpatDist maybeSpatDistMap
            spatDistsKM = spatDist/1000
        -- filter by distance
            filtered = case maybeSpaceTimeFilter of
                Just (spaceFilter,timeFilter) -> spatDistsKM > spaceFilter || tempDist > timeFilter
                Nothing -> False
        in if filtered
           then return Nothing
           else do
                weightsPerDepvar <- mapM (weightPerDepVar kernelDefinition [spatDistsKM,tempDist]) depVars
                return $ Just $ ObsWithWeights obs (IndepSpatTempDist (SpatTempDist spatDistsKM tempDist)) (DepVarsPos weightsPerDepvar)
        where
            findTempDist :: Maybe TempSampleMatrix -> Double
            -- calculate distances from mean ages
            findTempDist Nothing = temporalDistSpatTempPos gridSpatTempPos obsSpatTempPos
            -- look up age samples and calculate distances from them
            findTempDist (Just tempSampleMatrix) =
                let (SpatTempPos _ (TempPos gridPointAge)) = gridSpatTempPos
                    obsAgeSample = lookUpTempSample tempSampleMatrix tempSampIteration obsIndex
                in temporalDistYearBCAD gridPointAge obsAgeSample
            findSpatDist :: Maybe SpatDistMatrix -> Double
            -- calculate distances
            findSpatDist Nothing = spatialDistSpatTempPos gridSpatTempPos obsSpatTempPos
            -- look up distances
            findSpatDist (Just spatDistMatrix) =
                let gridSpatPosIndex = getIndex $ _spatialPos gridSpatTempPos
                in lookUpDistanceAU spatDistMatrix gridSpatPosIndex obsIndex
-- arbitrary dim distances
getWeightsPerObs
    _
    (CorePermutation (IndepArbitraryDimPos gridAbritryDimPos) _ kernelDefinition _)
    depVars
    obs@(Observation _ _ (HyperPos (IndepArbitraryDimPos obsArbitraryDimPos) _)) = do
        let indepVarNames = getKeys obsArbitraryDimPos
            obsPos = getValues obsArbitraryDimPos
            gridPos = getValues gridAbritryDimPos
            dists = allDistances obsPos gridPos
        weightsPerDepvar <- mapM (weightPerDepVar kernelDefinition dists) depVars
        return $ Just $ ObsWithWeights obs (IndepArbitraryDimDist (ArbitraryDimPos $ zip indepVarNames dists)) (DepVarsPos weightsPerDepvar)
-- wrong input
getWeightsPerObs _ _ _ _ = pure Nothing

weightPerDepVar :: KernelDefinition -> [Double] -> DepVarName -> CoreLog (DepVarName, Double)
weightPerDepVar (KernelDefinition kernelsPerDepVar) dists depVar  = do
    (shape,nugget,lengths) <- getKernelForOneDepVar
    let sqWeiDist = foldSum (zipWith (\d t -> (d / t) ** 2) dists (getValues lengths))
    let weight = weightByKernel shape nugget sqWeiDist
    return (depVar, weight)
    where
        weightByKernel :: KernelShape -> KernelNugget -> Double -> Double
        weightByKernel SquaredExponential nugget d = nugget / (nugget + exp d - 1)
        weightByKernel Linear             nugget d = nugget / (nugget + sqrt d)
        getKernelForOneDepVar :: CoreLog (KernelShape, KernelNugget, KernelLengths)
        getKernelForOneDepVar = do
            case filter (\(KernelOneDepVar name _ _ _) -> name == depVar) kernelsPerDepVar of
                []                    -> E.throwError $ NormalException "Variable not defined in kernel definition"
                [KernelOneDepVar _ s n k] -> pure (s, n, k)
                _                     -> E.throwError $ NormalException "Variable defined multiple times in kernel definition"

interpolAndSearchOneDepVar :: V.Vector ObsWithWeights -> (DepVarName, Maybe Double) -> CoreLog InterpolationResultOneDepVar
interpolAndSearchOneDepVar obsWithWeights (depVar,maybeValueDepVar) = do
    values <- VU.convert <$> V.mapM (getDepVarValuePerObs depVar) obsWithWeights
    weights <- VU.convert <$> V.mapM (getDepVarWeightPerObs depVar) obsWithWeights
    let totalWeight = VU.sum weights
        neff        = totalWeight
        weightedA   = weightedAvg_ totalWeight values weights
        weightedV   = weightedVar_ totalWeight weightedA values weights
    case posteriorPredictive_ totalWeight weightedA weightedV of
        Right distribution -> do
            let lower  = quantile distribution 0.025
                median = quantile distribution 0.5 -- I'm sure now this is identical to weightedA
                upper  = quantile distribution 0.975
                prob   = fmap (density distribution) maybeValueDepVar
            return $ InterpolationResultOneDepVarFull depVar neff weightedA weightedV (OutBool True) (OutInfDouble lower) median (OutInfDouble upper) prob
        Left _ -> do
            case maybeValueDepVar of
                Just _ ->
                    -- is setting the probability to 0 a good idea?
                    return $ InterpolationResultOneDepVarFull depVar neff weightedA weightedV (OutBool False) (OutInfDouble (-infinity)) weightedA (OutInfDouble infinity) (Just 0)
                Nothing ->
                    return $ InterpolationResultOneDepVarFull depVar neff weightedA weightedV (OutBool False) (OutInfDouble (-infinity)) weightedA (OutInfDouble infinity) Nothing

getDepVarValuePerObs :: DepVarName -> ObsWithWeights -> CoreLog Double
getDepVarValuePerObs depVar (ObsWithWeights (Observation _ _ (HyperPos _ (DepVarsPos m))) _ _) =
    case lookup depVar m of
        Nothing -> E.throwError $ NormalException "Unknown variable"
        Just x  -> pure x

getDepVarWeightPerObs :: DepVarName -> ObsWithWeights -> CoreLog Double
getDepVarWeightPerObs depVar (ObsWithWeights _ _ (DepVarsPos m)) =
    case lookup depVar m of
        Nothing -> E.throwError $ NormalException "Unknown variable"
        Just x  -> pure x

module LocEst.CoreAlgorithms where

import           LocEst.Distance
import           LocEst.Exceptions
import           LocEst.MathUtils
import           LocEst.Types

import           Data.List               (sortBy)
import           Data.Maybe              (catMaybes, mapMaybe)
import qualified Data.Vector             as V
import qualified Data.Vector.Unboxed     as VU
import           Statistics.Distribution (logDensity, quantile)

-- weights-per-obs application
coreOutObsWeight :: Double -> Int -> CoreSupplement -> [DepVarName] 
                    -> V.Vector Observation -> CorePermutation -> V.Vector ObsWeight
coreOutObsWeight spatDistUnitScaling nrTopObs
    (CoreSupplement spaceTimeMinFilter spaceTimeMaxFilter maybeSpatDistMap maybeTempSamples)
     depVars observations sett@(CorePermutation _ _ kernelDefinition _ _) =
    let dists = V.map (getDists spatDistUnitScaling maybeSpatDistMap maybeTempSamples sett) observations
        obsWithDistFiltered = V.filter (inFilterRange spaceTimeMinFilter spaceTimeMaxFilter) $ V.zip observations dists
        kernelsPerDepVar = map (lookupUnsafe kernelDefinition) depVars
        weights = V.map
            (\obs -> ValuesPerDepVar $ zipWith
                (\depVar kernelPerDepVar -> (depVar, getWeightOneObsOneDepVar kernelPerDepVar obs))
                depVars kernelsPerDepVar)
            obsWithDistFiltered
        obsWithWeights = V.zipWith (\(x,y) z -> ObsWithWeights x y z) obsWithDistFiltered weights
        obsWithWeightsSubset = V.fromList $ take nrTopObs $ sortBy (flip compareObsWithWeights) $ V.toList obsWithWeights
    in V.map (ObsWeight sett) obsWithWeightsSubset

-- random interpolation sampling application
coreOutInterpolSamples :: Double -> DepVarVariances -> CoreSupplement -> [DepVarName]
                          -> V.Vector Observation -> (CorePermutation, [(Int, DepVarsRands)]) -> V.Vector InterpolationSample
coreOutInterpolSamples spatDistUnitScaling depVarVariances
    (CoreSupplement spaceTimeMinFilter spaceTimeMaxFilter maybeSpatDistMap maybeTempSamples)
     depVars observations (sett@(CorePermutation _ _ kernelDefinition _ _), randIterations) =
    let dists = V.map (getDists spatDistUnitScaling maybeSpatDistMap maybeTempSamples sett) observations
        obsWithDistFiltered = V.filter (inFilterRange spaceTimeMinFilter spaceTimeMaxFilter) $ V.zip observations dists
        kernelsPerDepVar = map (lookupUnsafe kernelDefinition) depVars
        samplesPerDepVar = map (\(i,r) -> (i, zipWith (getRandomSampleOneDepVar obsWithDistFiltered r depVarVariances) depVars kernelsPerDepVar)) randIterations
    in V.fromList $ map (\(i,s) -> InterpolationSample sett i (ValuesPerDepVar s)) samplesPerDepVar

-- interpolation and search application
coreNormal :: Double -> CoreOutMode -> DepVarVariances -> CoreSupplement -> [DepVarName] -> V.Vector Observation -> CorePermutation -> SearchResult
coreNormal spatDistUnitScaling outMode depVarVariances
    (CoreSupplement spaceTimeMinFilter spaceTimeMaxFilter maybeSpatDistMap maybeTempSamples)
     depVars observations sett@(CorePermutation _ searchDepVarPos kernelDefinition _ _) =
    let dists = V.map (getDists spatDistUnitScaling maybeSpatDistMap maybeTempSamples sett) observations
        obsWithDistFiltered = V.filter (inFilterRange spaceTimeMinFilter spaceTimeMaxFilter) $ V.zip observations dists
        kernelsPerDepVar = map (lookupUnsafe kernelDefinition) depVars
        valuePerDepVar = case searchDepVarPos of
            Just (DepVarsPredPosDirect x)    -> Just <$> getValues x
            Just (DepVarsPredPosSearchObs x) -> Just <$> getValues ((_hyposDepVarsPos . _obsPos) x)
            Nothing                          -> replicate (length depVars) Nothing
        interpolPerDepVarFull = zipWith3 (interpolAndSearchOneDepVar obsWithDistFiltered depVarVariances) depVars kernelsPerDepVar valuePerDepVar
        interpolPerDepVar = case outMode of
            CoreOutShort -> map resOneDepvar2Short interpolPerDepVarFull
            CoreOutFull  -> interpolPerDepVarFull
            _            -> undefined
    in SearchResult {
           _srCorePermutation = sett
         , _srInterpolation   = InterpolationResult interpolPerDepVar
         , _srLikelihood      = case mapMaybe getLogLikelihood interpolPerDepVarFull of
            [] -> Nothing
            xs ->
                let valuesPerDepVar = catMaybes valuePerDepVar
                    depDist = euclideanDistance (map _irodvWeightedAvg interpolPerDepVarFull) valuesPerDepVar
                in Just SearchLikelihood {
                  _slhEuclideanDep  = depDist
                , _slhLogLikelihood = foldSum xs -- sum, not product, because log-likelihood
                , _slhProbability   = Nothing
                }
         }

compareObsWithWeights :: ObsWithWeights -> ObsWithWeights -> Ordering
compareObsWithWeights (ObsWithWeights _ _ (ValuesPerDepVar x1)) (ObsWithWeights _ _ (ValuesPerDepVar x2)) =
    compare (foldSum (map snd x1)) (foldSum (map snd x2))

getDists ::
       Double
    -> Maybe SpatDistMatrix
    -> Maybe TempSampleMatrix
    -> CorePermutation
    -> Observation
    -> IndepVarsDist
-- spatiotemporal distances
getDists
    spatDistUnitScaling
    maybeSpatDistMap maybeTempSamples
    (CorePermutation (IndepSpatTempPos gridSpatTempPos) _ _ tempSampIteration _)
    (Observation obsIndex _ (HyperPos (IndepSpatTempPos obsSpatTempPos) _) _) =
        let spatDist = findSpatDist maybeSpatDistMap
            spaceDistScaled = spatDist * spatDistUnitScaling
            tempDist = findTempDist maybeTempSamples
        in IndepSpatTempDist (SpatTempDist spaceDistScaled tempDist)
        where
            -- temporal distances
            findTempDist :: Maybe TempSampleMatrix -> Double
            -- calculate distances from mean ages
            findTempDist Nothing = temporalDistSpatTempPos gridSpatTempPos obsSpatTempPos
            -- look up age samples and calculate distances from them
            findTempDist (Just tempSampleMatrix) =
                let (SpatTempPos _ (TempPos gridPointAge)) = gridSpatTempPos
                    obsAgeSample = lookUpTempSample tempSampleMatrix tempSampIteration obsIndex
                in temporalDistYearBCAD gridPointAge obsAgeSample
            -- spatial distances
            findSpatDist :: Maybe SpatDistMatrix -> Double
            -- calculate distances
            findSpatDist Nothing = spatialDistSpatTempPos gridSpatTempPos obsSpatTempPos
            -- look up distances
            findSpatDist (Just spatDistMatrix) =
                let gridSpatPosIndex = getIndex $ _spatialPos gridSpatTempPos
                in lookUpDistanceAU spatDistMatrix gridSpatPosIndex obsIndex
-- arbitrary dim distances
getDists
    _
    _ _
    (CorePermutation (IndepArbitraryDimPos gridAbritryDimPos) _ _ _ _)
    (Observation _ _ (HyperPos (IndepArbitraryDimPos obsArbitraryDimPos) _) _) =
        let keys = getKeys obsArbitraryDimPos
            obsPos  = getValues obsArbitraryDimPos
            gridPos = getValues gridAbritryDimPos
            arbitraryDimDist = ValuesPerIndepVar $ zip keys (allDistances obsPos gridPos)
        in IndepArbitraryDimDist arbitraryDimDist
-- wrong input
getDists _ _ _ _ _ = throwL "mismatch of independent variable definitions in distance calculation"

inFilterRange :: (Double, Double) -> (Double, Double) -> (Observation, IndepVarsDist) -> Bool
inFilterRange
    (spaceMinFilter,timeMinFilter)
    (spaceMaxFilter,timeMaxFilter)
    (_,IndepSpatTempDist (SpatTempDist spatDistsKM tempDist)) =
    spatDistsKM <= spaceMaxFilter && spatDistsKM >= spaceMinFilter &&
    tempDist <= timeMaxFilter && tempDist >= timeMinFilter
inFilterRange _ _ _ = True

getRandomSampleOneDepVar ::
       V.Vector (Observation, IndepVarsDist)
    -> DepVarsRands
    -> DepVarVariances
    -> DepVarName
    -> KernelOneDepVar
    -> (DepVarName, Double)
getRandomSampleOneDepVar obsWithDist depVarsRands depVarVariances depVar kernelPerDepVar = do
    let values  = VU.convert $ V.map (getValueOneObsOneDepVar depVar) obsWithDist
        weights = VU.convert $ V.map (getWeightOneObsOneDepVar kernelPerDepVar) obsWithDist
        random01 = lookupUnsafe depVarsRands depVar
        sampleVariance = lookupUnsafe depVarVariances depVar
        totalWeight = VU.sum weights
        weightedA   = weightedAvg_ totalWeight values weights
        weightedVBasic = weightedVarBasic_ totalWeight weightedA values weights
        weightedV   = weightedVar_ sampleVariance weightedVBasic totalWeight
    case posteriorPredictive_ totalWeight weightedA weightedV of
        Right distribution -> (depVar, quantile distribution random01)
        Left _             -> (depVar,nan)

interpolAndSearchOneDepVar ::
       V.Vector (Observation, IndepVarsDist)
    -> DepVarVariances
    -> DepVarName
    -> KernelOneDepVar
    -> Maybe Double
    -> InterpolationResultOneDepVar
interpolAndSearchOneDepVar obsWithDist depVarVariances depVar kernelPerDepVar maybeValueDepVar = do
    let values  = VU.convert $ V.map (getValueOneObsOneDepVar depVar) obsWithDist
        weights = VU.convert $ V.map (getWeightOneObsOneDepVar kernelPerDepVar) obsWithDist
        sampleVariance = lookupUnsafe depVarVariances depVar
        totalWeight = VU.sum weights
        neff        = totalWeight
        weightedA   = weightedAvg_ totalWeight values weights
        weightedVBasic = weightedVarBasic_ totalWeight weightedA values weights
        weightedV   = weightedVar_ sampleVariance weightedVBasic totalWeight
    case posteriorPredictive_ totalWeight weightedA weightedV of
        Right distribution ->
            let lower  = quantile distribution 0.025
                median = quantile distribution 0.5 -- this is identical to weightedA
                upper  = quantile distribution 0.975
                logL   = fmap (logDensity distribution) maybeValueDepVar -- log-likelihood
            in InterpolationResultOneDepVarFull
                depVar neff weightedA weightedVBasic weightedV (OutBool True)
                (OutInfDouble lower) median (OutInfDouble upper) logL
        Left _ ->
            case maybeValueDepVar of
                Just _ ->
                    InterpolationResultOneDepVarFull
                        depVar neff weightedA weightedVBasic weightedV (OutBool False)
                        (OutInfDouble (-infinity)) weightedA (OutInfDouble infinity) (Just (-infinity)) -- requires a proper prior
                Nothing ->
                    InterpolationResultOneDepVarFull
                        depVar neff weightedA weightedVBasic weightedV (OutBool False)
                        (OutInfDouble (-infinity)) weightedA (OutInfDouble infinity) Nothing

getValueOneObsOneDepVar :: DepVarName -> (Observation,IndepVarsDist) -> Double
getValueOneObsOneDepVar depVar (Observation _ _ (HyperPos _ depVarsPos) _, _ ) = lookupUnsafe depVarsPos depVar

getWeightOneObsOneDepVar ::
       KernelOneDepVar
    -> (Observation, IndepVarsDist)
    -> Double
getWeightOneObsOneDepVar (KernelOneDepVar _ shape nugget lengths) (_,dists) =
    weightForOneObs shape nugget (squaredWeightedDistForOneObs lengths dists)
    where
        weightForOneObs :: KernelShape -> KernelNugget -> Double -> Double
        weightForOneObs SquaredExponential n d = n / (n + exp d - 1)
        weightForOneObs Linear             n d = n / (n + sqrt d)
        squaredWeightedDistForOneObs :: KernelLengths -> IndepVarsDist -> Double
        squaredWeightedDistForOneObs
            (KernelLengths (ValuesPerIndepVar [(_,spaceKernelWidth), (_,timeKernelWidth)]))
            (IndepSpatTempDist (SpatTempDist spatDist tempDist)) =
            (spatDist / spaceKernelWidth) ** 2 + (tempDist / timeKernelWidth) ** 2
        squaredWeightedDistForOneObs
            kernLengths
            (IndepArbitraryDimDist namedDists) =
            let distances = getValues namedDists
                thetas    = getValues kernLengths
            in foldSum (zipWith (\d t -> (d / t) ** 2) distances thetas)
        squaredWeightedDistForOneObs _ _ =
            throwL "mismatch of independent variable definitions in weight calculation"

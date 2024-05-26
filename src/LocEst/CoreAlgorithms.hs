module LocEst.CoreAlgorithms where

import           LocEst.Distance
import           LocEst.MathUtils
import           LocEst.Types
import           LocEst.Exceptions

import           Data.List               (find, sortBy)
import           Data.Maybe              (mapMaybe, catMaybes)
import qualified Data.Vector             as V
import qualified Data.Vector.Unboxed     as VU
import           Statistics.Distribution (quantile, logDensity)

core ::
       CoreOutMode
    -> CoreSupplement
    -> V.Vector Observation
    -> CorePermutation
    -> CoreOut
core (CoreOutObsWeight nrTopObs)
    (CoreSupplement maybeSpaceTimeFilter maybeSpatDistMap maybeTempSamples)
     observations sett@(CorePermutation _ _ kernelDefinition _) =
    let depVars = getKeys kernelDefinition
        dists = V.map (getDists maybeSpatDistMap maybeTempSamples sett) observations
        obsWithDistFiltered = V.filter (inFilterRange maybeSpaceTimeFilter) $ V.zip observations dists
        kernelsPerDepVar = map (getKernelForOneDepVar kernelDefinition) depVars
        weights = V.map
            (\obs -> ValuesPerDepVar $ zipWith
                (\depVar kernelPerDepVar -> (depVar, getWeightOneObsOneDepVar kernelPerDepVar obs))
                depVars kernelsPerDepVar)
            obsWithDistFiltered
        obsWithWeights = V.zipWith (\(x,y) z -> ObsWithWeights x y z) obsWithDistFiltered weights
        obsWithWeightsSubset = V.fromList $ take nrTopObs $ sortBy (flip compareObsWithWeights) $ V.toList obsWithWeights
    in CoreObsWeight (V.map (ObsWeight sett) obsWithWeightsSubset)
core
    outMode
    (CoreSupplement maybeSpaceTimeFilter maybeSpatDistMap maybeTempSamples)
     observations sett@(CorePermutation _ searchDepVarPos kernelDefinition _) =
    let depVars = getKeys kernelDefinition
        dists = V.map (getDists maybeSpatDistMap maybeTempSamples sett) observations
        obsWithDistFiltered = V.filter (inFilterRange maybeSpaceTimeFilter) $ V.zip observations dists
        kernelsPerDepVar = map (getKernelForOneDepVar kernelDefinition) depVars
        valuePerDepVar = case searchDepVarPos of
            Just (DepVarsPredPosDirect x)    -> Just <$> getValues x
            Just (DepVarsPredPosSearchObs x) -> Just <$> getValues ((_hyposDepVarsPos . _obsPos) x)
            Nothing                          -> replicate (length depVars) Nothing
        interpolPerDepVarFull = zipWith3 (interpolAndSearchOneDepVar obsWithDistFiltered) depVars kernelsPerDepVar valuePerDepVar
        interpolPerDepVar = case outMode of
            CoreOutShort -> map resOneDepvar2Short interpolPerDepVarFull
            CoreOutFull  -> interpolPerDepVarFull
    in CoreSearchResult $ SearchResult {
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
       Maybe SpatDistMatrix
    -> Maybe TempSampleMatrix
    -> CorePermutation
    -> Observation
    -> IndepVarsDist
-- spatiotemporal distances
getDists
    maybeSpatDistMap maybeTempSamples
    (CorePermutation (IndepSpatTempPos gridSpatTempPos) _ _ tempSampIteration)
    (Observation obsIndex _ (HyperPos (IndepSpatTempPos obsSpatTempPos) _)) =
        let spatDist = findSpatDist maybeSpatDistMap
            spatDistsKM = spatDist/1000
            tempDist = findTempDist maybeTempSamples
        in IndepSpatTempDist (SpatTempDist spatDistsKM tempDist)
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
getDists
    _ _
    (CorePermutation (IndepArbitraryDimPos gridAbritryDimPos) _ _ _)
    (Observation _ _ (HyperPos (IndepArbitraryDimPos obsArbitraryDimPos) _)) =
        let keys = getKeys obsArbitraryDimPos
            obsPos = getValues obsArbitraryDimPos
            gridPos = getValues gridAbritryDimPos
            arbitraryDimDist = ValuesPerIndepVar $ zip keys (allDistances obsPos gridPos)
        in IndepArbitraryDimDist arbitraryDimDist
-- wrong input
getDists _ _ _ _ = throwL "Should not happen" -- ToDo

inFilterRange :: Maybe (Double, Double) -> (Observation, IndepVarsDist) -> Bool
inFilterRange
    (Just (spaceFilter,timeFilter))
    (_,IndepSpatTempDist (SpatTempDist spatDistsKM tempDist)) =
    spatDistsKM <= spaceFilter && tempDist <= timeFilter
inFilterRange _ _ = True

getKernelForOneDepVar :: KernelDefinition -> String -> (KernelShape, KernelNugget, KernelLengths)
getKernelForOneDepVar (KernelDefinition kernelsPerDepVar) depVar = do
    case find (\(KernelOneDepVar name _ _ _) -> name == depVar) kernelsPerDepVar of
        Just (KernelOneDepVar _ s n k) -> (s, n, k)
        Nothing                        -> throwL "Variable not defined in kernel definition"

interpolAndSearchOneDepVar ::
       V.Vector (Observation, IndepVarsDist)
    -> DepVarName
    -> (KernelShape, KernelNugget, KernelLengths)
    -> Maybe Double
    -> InterpolationResultOneDepVar
interpolAndSearchOneDepVar obsWithDist depVar kernelPerDepVar maybeValueDepVar = do
    let values  = VU.convert $ V.map (getValueOneObsOneDepVar depVar) obsWithDist
        weights = VU.convert $ V.map (getWeightOneObsOneDepVar kernelPerDepVar) obsWithDist
        totalWeight = VU.sum weights
        neff        = totalWeight
        weightedA   = weightedAvg_ totalWeight values weights
        weightedV   = weightedVar_ totalWeight weightedA values weights
    case posteriorPredictive_ totalWeight weightedA weightedV of
        Right distribution ->
            let lower  = quantile distribution 0.025
                median = quantile distribution 0.5 -- this is identical to weightedA
                upper  = quantile distribution 0.975
                logL   = fmap (logDensity distribution) maybeValueDepVar -- log-likelihood
            in InterpolationResultOneDepVarFull
                depVar neff weightedA weightedV (OutBool True)
                (OutInfDouble lower) median (OutInfDouble upper) logL
        Left _ ->
            case maybeValueDepVar of
                Just _ ->
                    -- is setting the probability to 0 a good idea?
                    InterpolationResultOneDepVarFull
                        depVar neff weightedA weightedV (OutBool False)
                        (OutInfDouble (-infinity)) weightedA (OutInfDouble infinity) (Just 0)
                Nothing ->
                    InterpolationResultOneDepVarFull
                        depVar neff weightedA weightedV (OutBool False)
                        (OutInfDouble (-infinity)) weightedA (OutInfDouble infinity) Nothing

getValueOneObsOneDepVar :: DepVarName -> (Observation,IndepVarsDist) -> Double
getValueOneObsOneDepVar depVar (Observation _ _ (HyperPos _ (ValuesPerDepVar m)), _) =
    case lookup depVar m of
        Just x  -> x
        Nothing -> throwL "Unknown variable"

getWeightOneObsOneDepVar ::
       (KernelShape, KernelNugget, KernelLengths)
    -> (Observation, IndepVarsDist)
    -> Double
getWeightOneObsOneDepVar kernelPerDepVar (_,dists) =
    let (shape,nugget,lengths) = kernelPerDepVar
        sqWeiDist = squaredWeightedDistForOneObs lengths dists
    in weightForOneObs shape nugget sqWeiDist
    where
        weightForOneObs :: KernelShape -> KernelNugget -> Double -> Double
        weightForOneObs SquaredExponential nugget d = nugget / (nugget + exp d - 1)
        weightForOneObs Linear             nugget d = nugget / (nugget + sqrt d)
        squaredWeightedDistForOneObs :: KernelLengths -> IndepVarsDist -> Double
        squaredWeightedDistForOneObs
            (KernelLengths (ValuesPerIndepVar [(_,spaceKernelWidth), (_,timeKernelWidth)]))
            (IndepSpatTempDist (SpatTempDist spatDist tempDist)) =
            (spatDist / spaceKernelWidth) ** 2 + (tempDist / timeKernelWidth) ** 2
        squaredWeightedDistForOneObs
            lengths
            (IndepArbitraryDimDist namedDists) =
            let ds = getValues namedDists
            in foldSum (zipWith (\d t -> (d / t) ** 2) ds (getValues lengths))
        squaredWeightedDistForOneObs _ _ =
            throwL "Illegal combination of kernel and grid data"

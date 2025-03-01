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
coreOutObsWeight spatDistUnitScaling nrTopObs coreSupplement
     depVars observations sett@(CorePermutation _ _ kernelDefinition _ _) =
    let obsWithDistFiltered = getObsWithDist spatDistUnitScaling coreSupplement sett observations
        kernelsPerDepVar = map (lookupUnsafe kernelDefinition) depVars
        weights = V.map
            (\obs -> ValuesPerDepVar $ zipWith
                (\depVar kernelPerDepVar -> (depVar, getWeight kernelPerDepVar obs))
                depVars kernelsPerDepVar)
            obsWithDistFiltered
        obsWithWeights = V.zipWith (\(x,y) z -> ObsWithWeights x y z) obsWithDistFiltered weights
        obsWithWeightsSubset = V.fromList $ take nrTopObs $ sortBy (flip compare) $ V.toList obsWithWeights
    in V.map (ObsWeight sett) obsWithWeightsSubset

-- random interpolation sampling application
coreOutInterpolSamples :: Double -> DepVarVariances -> CoreSupplement -> [DepVarName]
                          -> V.Vector Observation -> (CorePermutation, [(Int, DepVarsRands)]) -> V.Vector InterpolationSample
coreOutInterpolSamples spatDistUnitScaling depVarVariances coreSupplement
     depVars observations (sett@(CorePermutation _ _ kernelDefinition _ _), randIterations) =
    let obsWithDistFiltered = getObsWithDist spatDistUnitScaling coreSupplement sett observations
        kernelsPerDepVar = map (lookupUnsafe kernelDefinition) depVars
        samplesPerDepVar = map (\(i,r) -> (i, zipWith (getRandomSample obsWithDistFiltered r depVarVariances) depVars kernelsPerDepVar)) randIterations
    in V.fromList $ map (\(i,s) -> InterpolationSample sett i (ValuesPerDepVar s)) samplesPerDepVar

-- interpolation and search application
coreNormal :: Double -> CoreOutMode -> DepVarVariances -> CoreSupplement -> [DepVarName]
              -> V.Vector Observation -> CorePermutation -> SearchResult
coreNormal spatDistUnitScaling outMode depVarVariances coreSupplement
     depVars observations sett@(CorePermutation _ searchDepVarPos kernelDefinition _ _) =
    let obsWithDistFiltered = getObsWithDist spatDistUnitScaling coreSupplement sett observations
        kernelsPerDepVar = map (lookupUnsafe kernelDefinition) depVars
        valuePerDepVar = case searchDepVarPos of
            Just (DepVarsPredPosDirect x)    -> Just <$> getValues x
            Just (DepVarsPredPosSearchObs x) -> Just <$> getValues ((_hyposDepVarsPos . _obsPos) x)
            Nothing                          -> replicate (length depVars) Nothing
        interpolPerDepVarFull = zipWith3 (interpolAndSearch obsWithDistFiltered depVarVariances) depVars kernelsPerDepVar valuePerDepVar
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

getRandomSample ::
       V.Vector (Observation, IndepVarsDist)
    -> DepVarsRands
    -> DepVarVariances
    -> DepVarName
    -> KernelOneDepVar
    -> (DepVarName, Double)
getRandomSample obsWithDist depVarsRands depVarVariances depVar kernelPerDepVar = do
    let values  = VU.convert $ V.map (getValue depVar) obsWithDist
        weights = VU.convert $ V.map (getWeight kernelPerDepVar) obsWithDist
        random01 = lookupUnsafe depVarsRands depVar
        sampleVariance = lookupUnsafe depVarVariances depVar
        totalWeight = VU.sum weights
        weightedA   = weightedAvg_ totalWeight values weights
        weightedVBasic = weightedVarBasic_ totalWeight weightedA values weights
        weightedV   = weightedVar_ sampleVariance weightedVBasic totalWeight
    case posteriorPredictive_ totalWeight weightedA weightedV of
        Right distribution -> (depVar, quantile distribution random01)
        Left _             -> (depVar,nan)

interpolAndSearch ::
       V.Vector (Observation, IndepVarsDist)
    -> DepVarVariances
    -> DepVarName
    -> KernelOneDepVar
    -> Maybe Double
    -> InterpolationResultOneDepVar
interpolAndSearch obsWithDist depVarVariances depVar kernelPerDepVar maybeValueDepVar = do
    let values  = VU.convert $ V.map (getValue depVar) obsWithDist
        weights = VU.convert $ V.map (getWeight kernelPerDepVar) obsWithDist
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

getValue :: DepVarName -> (Observation,IndepVarsDist) -> Double
getValue depVar (Observation _ _ (HyperPos _ depVarsPos) _, _ ) = lookupUnsafe depVarsPos depVar

getWeight ::
       KernelOneDepVar
    -> (Observation, IndepVarsDist)
    -> Double
getWeight (KernelOneDepVar _ shape lengths) (_,dists) =
    weight shape (squaredWeightedDist lengths dists)
    where
        weight :: KernelShape -> Double -> Double
        weight SquaredExponential d = 1 / exp d
        weight Linear             d = 1 / (1 + sqrt d)
        squaredWeightedDist :: KernelLengths -> IndepVarsDist -> Double
        squaredWeightedDist
            (KernelLengths (ValuesPerIndepVar [(_,spaceKernelWidth), (_,timeKernelWidth)]))
            (IndepSpatTempDist (SpatTempDist spatDist tempDist)) =
            (spatDist / spaceKernelWidth) ** 2 + (tempDist / timeKernelWidth) ** 2
        squaredWeightedDist
            kernLengths
            (IndepArbitraryDimDist namedDists) =
            let distances = getValues namedDists
                thetas    = getValues kernLengths
            in foldSum (zipWith (\d t -> (d / t) ** 2) distances thetas)
        squaredWeightedDist _ _ =
            throwL "mismatch of independent variable definitions in weight calculation"

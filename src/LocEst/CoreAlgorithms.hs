module LocEst.CoreAlgorithms where

import           LocEst.Distance
import           LocEst.Exceptions
import           LocEst.MathUtils
import           LocEst.Types

import           Data.Bifunctor          (second)
import           Data.List               (sortBy, zipWith4)
import           Data.Maybe              (catMaybes, mapMaybe)
import qualified Data.Vector             as V
import qualified Data.Vector.Unboxed     as VU
import           Statistics.Distribution (logDensity, quantile)

-- weights-per-obs application
coreOutObsWeight :: Double -> Int -> CoreSupplement -> [DepVarName]
                    -> V.Vector Observation -> CorePermutation
                    -> V.Vector ObsWeight
coreOutObsWeight spatDistUnitScaling nrTopObs coreSupplement
     depVars observations sett@(CorePermutation _ _ kernelDefinition _ _) =
    let obsWithDist      = filterObs spatDistUnitScaling coreSupplement sett observations
        kernelsPerDepVar = map (lookupUnsafe kernelDefinition) depVars
        weights = V.map
            (\obs -> ValuesPerDepVar $ zipWith
                (\depVar kernelPerDepVar -> (depVar, getWeight kernelPerDepVar obs))
                depVars kernelsPerDepVar)
            obsWithDist
        obsWithWeights = V.zipWith (\(x,y) z -> ObsWithWeights x y z) obsWithDist weights
        obsWithWeightsSubset = V.fromList $ take nrTopObs $ sortBy (flip compare) $ V.toList obsWithWeights
    in V.map (ObsWeight sett) obsWithWeightsSubset

-- random interpolation sampling application
coreOutInterpolSamples :: Double -> DepVarVariances -> CoreSupplement -> [DepVarName]
                          -> V.Vector Observation -> (CorePermutation, [(Int, DepVarsRands)])
                          -> V.Vector InterpolationSample
coreOutInterpolSamples spatDistUnitScaling depVarVariances coreSupplement
     depVars observations (sett@(CorePermutation _ _ kernelDefinition _ _), randIterations) =
    let obsWithDist        = filterObs spatDistUnitScaling coreSupplement sett observations
        kernelsPerDepVar   = map (lookupUnsafe kernelDefinition) depVars
        variancesPerDepVar = map (lookupUnsafe depVarVariances) depVars
        samplesPerDepVar   = map (second drawSamples) randIterations
        drawSamples r      = ValuesPerDepVar $
            zipWith3 (getRandomSample obsWithDist r) depVars kernelsPerDepVar variancesPerDepVar
    in V.fromList $ map (uncurry (InterpolationSample sett)) samplesPerDepVar

getRandomSample ::
       V.Vector (Observation, IndepVarsDist)
    -> DepVarsRands
    -> DepVarName
    -> KernelOneDepVar
    -> Double
    -> (DepVarName, Double)
getRandomSample obsWithDist depVarsRands depVar kernel variance = do
    let values      = VU.convert $ V.map (getValue depVar) obsWithDist
        weights     = VU.convert $ V.map (getWeight kernel) obsWithDist
        random01    = lookupUnsafe depVarsRands depVar
        totalWeight = VU.sum weights
        weightedA   = weightedAvg_ totalWeight values weights
        weightedVB  = weightedVarBasic_ totalWeight weightedA values weights
        weightedV   = weightedVar_ variance weightedVB totalWeight
    case posteriorPredictive_ totalWeight weightedA weightedV of
        Right distribution -> (depVar, quantile distribution random01)
        Left _             -> (depVar,nan)

-- interpolation and search application
coreNormal :: Double -> CoreOutMode -> DepVarVariances -> CoreSupplement -> [DepVarName]
              -> V.Vector Observation -> CorePermutation
              -> SearchResult
coreNormal spatDistUnitScaling outMode depVarVariances coreSupplement
     depVars observations sett@(CorePermutation _ searchDepVarPos kernelDefinition _ _) =
    let obsWithDist        = filterObs spatDistUnitScaling coreSupplement sett observations
        kernelsPerDepVar   = map (lookupUnsafe kernelDefinition) depVars
        variancesPerDepVar = map (lookupUnsafe depVarVariances) depVars
        searchPerDepVar    = case searchDepVarPos of
            Just (DepVarsPredPosDirect x)    -> Just <$> getValues x
            Just (DepVarsPredPosSearchObs x) -> Just <$> getValues ((_hyposDepVarsPos . _obsPos) x)
            Nothing                          -> replicate (length depVars) Nothing
        interpolPerDepVar = zipWith4 (interpol obsWithDist) depVars kernelsPerDepVar variancesPerDepVar searchPerDepVar
        interpolRes = case outMode of
            CoreOutShort -> map resOneDepvar2Short interpolPerDepVar
            CoreOutFull  -> interpolPerDepVar
            _            -> throwL "impossible outmode setting"
    in SearchResult {
           _srCorePermutation = sett
         , _srInterpolation   = InterpolationResult interpolRes
         , _srLikelihood      = case mapMaybe getLogLikelihood interpolRes of
            [] -> Nothing
            xs ->
                let valuesPerDepVar = catMaybes searchPerDepVar
                    depDist = euclideanDistance (map _irodvWeightedAvg interpolRes) valuesPerDepVar
                in Just SearchLikelihood {
                  _slhEuclideanDep  = depDist
                , _slhLogLikelihood = foldSum xs -- sum, not product, because log-likelihood
                , _slhProbability   = Nothing
                }
         }

interpol ::
       V.Vector (Observation, IndepVarsDist)
    -> DepVarName
    -> KernelOneDepVar
    -> Double
    -> Maybe Double
    -> InterpolationResultOneDepVar
interpol obsWithDist depVar kernel variance maybeSearchValue = do
    let values      = VU.convert $ V.map (getValue depVar) obsWithDist
        weights     = VU.convert $ V.map (getWeight kernel) obsWithDist
        totalWeight = VU.sum weights
        neff        = totalWeight
        weightedA   = weightedAvg_ totalWeight values weights
        weightedVB  = weightedVarBasic_ totalWeight weightedA values weights
        weightedV   = weightedVar_ variance weightedVB totalWeight
    case posteriorPredictive_ totalWeight weightedA weightedV of
        Right distribution ->
            let lower  = quantile distribution 0.025
                median = weightedA -- this is identical to: quantile distribution 0.5
                upper  = quantile distribution 0.975
                logL   = fmap (logDensity distribution) maybeSearchValue -- log-likelihood
            in InterpolationResultOneDepVarFull
                depVar neff weightedA weightedVB weightedV (OutBool True)
                (OutInfDouble lower) median (OutInfDouble upper) logL
        Left _ -> case maybeSearchValue of
            -- requires a proper prior
            Just _ -> InterpolationResultOneDepVarFull
                depVar neff weightedA weightedVB weightedV (OutBool False)
                (OutInfDouble (-infinity)) weightedA (OutInfDouble infinity) (Just (-infinity))
            Nothing -> InterpolationResultOneDepVarFull
                depVar neff weightedA weightedVB weightedV (OutBool False)
                (OutInfDouble (-infinity)) weightedA (OutInfDouble infinity) Nothing

getValue :: DepVarName -> (Observation,IndepVarsDist) -> Double
getValue depVar (Observation _ _ (HyperPos _ depVarsPos) _, _) = lookupUnsafe depVarsPos depVar

getWeight :: KernelOneDepVar -> (Observation, IndepVarsDist) -> Double
getWeight (KernelOneDepVar _ shape lengths) (_,dists) =
    computeWeight shape (squaredWeightedDist lengths dists)
    where
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

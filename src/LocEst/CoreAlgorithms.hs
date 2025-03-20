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
import qualified Numeric.LinearAlgebra as M

-- weights-per-obs application
coreOutObsWeight :: Double -> Int -> CoreSupplement -> [DepVarName]
                    -> V.Vector Observation -> CorePermutation
                    -> V.Vector ObsWeight
coreOutObsWeight spatDistUnitScaling nrTopObs coreSupplement
     depVars observations sett@(CorePermutation _ _ kernelDefinition _ _) =
    let (obs,dists)      = filterObs spatDistUnitScaling coreSupplement sett observations
        kernelsPerDepVar = getValues kernelDefinition
        weights = flip V.map dists $
            \dist -> ValuesPerDepVar $
                zipWith (\depVar kernelPerDepVar -> (depVar, getWeight kernelPerDepVar dist)) depVars kernelsPerDepVar
        obsWithWeights = V.zipWith3 ObsWithWeights obs dists weights
        obsWithWeightsSubset = V.fromList $ take nrTopObs $ sortBy (flip compare) $ V.toList obsWithWeights
    in V.map (ObsWeight sett) obsWithWeightsSubset

-- random interpolation sampling application
coreOutInterpolSamples :: Double -> DepVarVariances -> CoreSupplement -> [DepVarName]
                          -> V.Vector Observation -> (CorePermutation, [(Int, DepVarsRands)])
                          -> V.Vector InterpolationSample
coreOutInterpolSamples spatDistUnitScaling depVarVariances coreSupplement
     depVars observations (sett@(CorePermutation _ _ kernelDefinition _ _), randIterations) =
    let (obs,dists)        = filterObs spatDistUnitScaling coreSupplement sett observations
        kernelsPerDepVar   = getValues kernelDefinition
        variancesPerDepVar = getValues depVarVariances
        samplesPerDepVar   = map (second drawSamples) randIterations
        drawSamples r      = ValuesPerDepVar $
            zipWith3 (getRandomSample obs dists r) depVars kernelsPerDepVar variancesPerDepVar
    in V.fromList $ map (uncurry (InterpolationSample sett)) samplesPerDepVar

getRandomSample ::
       V.Vector Observation
    -> V.Vector IndepVarsDist
    -> DepVarsRands
    -> DepVarName
    -> KernelOneDepVar
    -> Double
    -> (DepVarName, Double)
getRandomSample obs dists depVarsRands depVar kernel variance = do
    let values      = VU.convert $ V.map (getDepVarsPos depVar) obs
        weights     = VU.convert $ V.map (getWeight kernel) dists
        random01    = lookupUnsafe depVarsRands depVar
        totalWeight = VU.sum weights
        weightedA   = weightedAvg_ totalWeight values weights
        weightedVB  = weightedVarBasic_ totalWeight weightedA values weights
        weightedV   = weightedVar_ variance weightedVB totalWeight
    case posteriorPredictive_ totalWeight weightedA weightedV of
        Right distribution -> (depVar, quantile distribution random01)
        Left _             -> (depVar,nan)


coreNormal2 :: Double -> CoreOutMode -> DepVarVariances -> CoreSupplement -> [DepVarName] -> [ M.Vector M.R]
              -> V.Vector Observation -> [CorePermutation] -> [SearchResult]
coreNormal2 spatDistUnitScaling outMode depVarVariances (CoreSupplement _ maybeSpatDistMap maybeTempSamples) depVars yPerDepVar observations permutations =
         let indepVarsPosGrid  = V.fromList $ map _casIndepVarsPos permutations
             tempSampIteration = head $ map _casTempSamplingIteration permutations
             kernelsPerDepVar  = getValues $ head $ map _casKernelDefinition permutations
             searchPerDepVar   = case head $ map _casSearchObs permutations of
                    Just (DepVarsPredPosDirect x)    -> Just <$> getValues x
                    Just (DepVarsPredPosSearchObs x) -> Just <$> getValues ((_hyposDepVarsPos . _obsPos) x)
                    Nothing                          -> replicate (length depVars) Nothing
             dists = pairwiseDists spatDistUnitScaling maybeSpatDistMap maybeTempSamples tempSampIteration observations indepVarsPosGrid
             res = zipWith3 (\y k s -> kas dists y k s) yPerDepVar kernelsPerDepVar searchPerDepVar
         in undefined

-- interpolation and search application
coreNormal :: Double -> CoreOutMode -> DepVarVariances -> CoreSupplement -> [DepVarName]
              -> V.Vector Observation -> CorePermutation
              -> SearchResult
coreNormal spatDistUnitScaling outMode depVarVariances coreSupplement
     depVars observations sett@(CorePermutation _ searchDepVarPos kernelDefinition _ _) =
    let (obs,dists)        = filterObs spatDistUnitScaling coreSupplement sett observations
        kernelsPerDepVar   = getValues kernelDefinition
        variancesPerDepVar = getValues depVarVariances
        searchPerDepVar    = case searchDepVarPos of
            Just (DepVarsPredPosDirect x)    -> Just <$> getValues x
            Just (DepVarsPredPosSearchObs x) -> Just <$> getValues ((_hyposDepVarsPos . _obsPos) x)
            Nothing                          -> replicate (length depVars) Nothing
        interpolPerDepVar = zipWith4 (interpol obs dists) depVars kernelsPerDepVar variancesPerDepVar searchPerDepVar
    in SearchResult {
           _srCorePermutation = sett
         , _srInterpolation   = case outMode of
                CoreOutShort -> InterpolationResult $ map resOneDepvar2Short interpolPerDepVar
                CoreOutFull  -> InterpolationResult interpolPerDepVar
                _            -> throwL "impossible outmode setting"
         , _srLikelihood      = case mapMaybe getLogLikelihood interpolPerDepVar of
                [] -> Nothing
                xs ->
                    let valuesPerDepVar = catMaybes searchPerDepVar
                        depDist = euclideanDistance (map _irodvWeightedAvg interpolPerDepVar) valuesPerDepVar
                    in Just SearchLikelihood {
                      _slhEuclideanDep  = depDist
                    , _slhLogLikelihood = foldSum xs -- sum, not product, because log-likelihood
                    , _slhProbability   = Nothing
                    }
         }

interpol ::
       V.Vector Observation
    -> V.Vector IndepVarsDist
    -> DepVarName
    -> KernelOneDepVar
    -> Double
    -> Maybe Double
    -> InterpolationResultOneDepVar
interpol obs dists depVar kernel variance maybeSearchValue = do
    let values      = VU.convert $ V.map (getDepVarsPos depVar) obs
        weights     = VU.convert $ V.map (getWeight kernel) dists
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

getWeight :: KernelOneDepVar -> IndepVarsDist -> Double
getWeight (KernelOneDepVar _ shape lengths) dists =
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

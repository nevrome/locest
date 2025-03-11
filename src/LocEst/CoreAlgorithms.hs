module LocEst.CoreAlgorithms where

import           LocEst.Distance
import           LocEst.Exceptions
import           LocEst.MathUtils
import           LocEst.Types

import           Data.Bifunctor          (second)
import           Data.List               (sortBy, zipWith5)
import           Data.Maybe              (catMaybes, mapMaybe)
import qualified Data.Vector             as V
import qualified Data.Vector.Unboxed     as VU
import           Statistics.Distribution (logDensity, quantile)


-- interpolation and search application
coreNormal :: Double -> CoreOutMode -> DepVarVariances -> CoreSupplement -> [DepVarName]
              -> V.Vector Observation -> CorePermutation
              -> SearchResult
coreNormal spatDistUnitScaling outMode depVarVariances coreSupplement
     depVars observations sett@(CorePermutation _ searchDepVarPos kernelDefinition _ _) =
    let obsWithDist        = filterObs spatDistUnitScaling coreSupplement sett observations
        kernelsPerDepVar   = getValues kernelDefinition
        depVarsIndizes = [0..(length kernelsPerDepVar - 1)]
        variancesPerDepVar = getValuesPerDepVar depVarVariances
        searchPerDepVar    = case searchDepVarPos of
            Just (DepVarsPredPosDirect x)    -> Just <$> getValuesPerDepVar x
            Just (DepVarsPredPosSearchObs x) -> Just <$> getValuesPerDepVar ((_hyposDepVarsPos . _obsPos) x)
            Nothing                          -> replicate (length depVars) Nothing
        interpolPerDepVar = zipWith5 (interpol obsWithDist) depVars depVarsIndizes kernelsPerDepVar variancesPerDepVar searchPerDepVar
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
       V.Vector (Observation, IndepVarsDist)
    -> DepVarName
    -> Int
    -> KernelOneDepVar
    -> Double
    -> Maybe Double
    -> InterpolationResultOneDepVar
interpol obsWithDist depVar depVarIndex kernel variance maybeSearchValue = do
    let values      = VU.convert $ V.map (getValue depVarIndex) obsWithDist
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

getValue :: Int -> (Observation,IndepVarsDist) -> Double
getValue depVar (Observation _ _ (HyperPos _ depVarsPos) _, _) = indexValuesPerDepVar depVarsPos depVar

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

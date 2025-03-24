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
import Numeric.LinearAlgebra as M
import qualified Data.Vector.Storable as VS
import Statistics.Distribution.Transform (LinearTransform)
import Statistics.Distribution.StudentT (StudentT)

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
         , _srInterpolation   = InterpolationResult interpolPerDepVar
         , _srLikelihood      = case mapMaybe getLogLikelihood interpolPerDepVar of
                [] -> Nothing
                xs ->
                    let valuesPerDepVar = catMaybes searchPerDepVar
                        depDist = euclideanDistance (map _irKASMedian interpolPerDepVar) valuesPerDepVar
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
    let values      = VS.convert $ V.map (getDepVarsPos depVar) obs
        weights     = M.fromRows [VS.convert $ V.map (getWeight kernel) dists]
    case kas weights values of
        (neff, wvb, wv, mu, Right distribution) ->
            let lower  = quantile distribution 0.025
                median = mu -- quantile distribution 0.5
                upper  = quantile distribution 0.975
                logL   = fmap (logDensity distribution) maybeSearchValue -- log-likelihood
            in KAS depVar neff wvb wv (OutBool True) (OutInfDouble lower) median (OutInfDouble upper) logL
        (neff, wvb, wv, mu, Left _) -> case maybeSearchValue of
            Just _ -> KAS depVar neff wvb wv (OutBool False) (OutInfDouble (-infinity)) mu (OutInfDouble infinity) (Just (-infinity))
            Nothing -> KAS depVar neff wvb wv (OutBool False) (OutInfDouble (-infinity)) mu (OutInfDouble infinity) Nothing

sumRows :: M.Matrix M.R -> M.Vector M.R
sumRows m = M.flatten $ m M.<> M.konst 1 (M.cols m, 1)

-- for this case application here weights is a vector
-- that simplifies the algorithm but it brings little gain in performance
-- I decided to keep the matrix version in case I want to refactor later
kas :: M.Matrix M.R -> M.Vector M.R -> (Double, Double, Double, Double, Either String (LinearTransform StudentT))
kas weights y = (
        totalWeight M.! 0,
        weightedVarBasic M.! 0,
        weightedVar M.! 0,
        mu M.! 0,
        generalizedStudentT (mu M.! 0) (scale M.! 0) (dof M.! 0)
    )
    where
      totalWeight = sumRows weights
      weightedAvg = M.flatten (weights M.<> M.asColumn y) / totalWeight
      values = M.fromRows $ replicate (M.rows weights) y
      weightedVarBasic = sumRows (weights * (values - M.asColumn weightedAvg) ** 2) / (totalWeight - 1)
      meanY = M.sumElements y / fromIntegral (M.size y)
      varSample = M.dot (y - M.scalar meanY) (y - M.scalar meanY) / fromIntegral (M.size y - 1)
      scaledS2 = (totalWeight - 1) * weightedVarBasic
      weightedVar = (scaledS2 + M.scalar varSample) / (totalWeight + 1)
      mu = weightedAvg
      scale = M.cmap sqrt ((1 + 1/totalWeight) * weightedVar)
      dof = totalWeight

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

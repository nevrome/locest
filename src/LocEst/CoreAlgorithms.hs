module LocEst.CoreAlgorithms where

import           LocEst.Distance
import           LocEst.Exceptions
import           LocEst.MathUtils
import           LocEst.Types

import           Data.Bifunctor                    (second)
import           Data.List                         (sortBy)
import           Data.Maybe                        (catMaybes, mapMaybe)
import qualified Data.Vector                       as V
import qualified Data.Vector.Storable              as VS
import qualified Data.Vector.Unboxed               as VU
import qualified Numeric.LinearAlgebra             as M
import           Statistics.Distribution           (logDensity, quantile)
import           Statistics.Distribution.StudentT  (StudentT)
import           Statistics.Distribution.Transform (LinearTransform)

-- weights-per-obs application
coreObsWeights :: Double -> Int -> Supplement -> [DepVarName]
               -> V.Vector Observation -> Permutation
               -> V.Vector ObsWeight
coreObsWeights spatDistUnitScaling nrTopObs coreSupplement
     depVars observations sett@(Permutation _ _ kernelDefinition _ _) =
    let (obs,dists)      = filterObs spatDistUnitScaling coreSupplement sett observations
        kernelsPerDepVar = getValues kernelDefinition
        weights = flip V.map dists $ \dist -> ValuesPerDepVar $
            zipWith (\depVar kernelPerDepVar -> (depVar, getWeight kernelPerDepVar dist))
                depVars kernelsPerDepVar
        obsWithWeights = V.zipWith3 ObsWithWeights obs dists weights
        obsWithWeightsSubset = V.fromList $ take nrTopObs $ sortBy (flip compare) $ V.toList obsWithWeights
    in V.map (ObsWeight sett) obsWithWeightsSubset

-- random interpolation sampling application
coreSamples :: Double -> Supplement -> [DepVarName]
            -> V.Vector Observation -> (Permutation, [(Int, DepVarsRands)])
            -> V.Vector InterpolationSample
coreSamples spatDistUnitScaling coreSupplement
     depVars observations (sett@(Permutation _ _ kernelDefinition _ _), randIterations) =
    let (obs,dists)      = filterObs spatDistUnitScaling coreSupplement sett observations
        kernelsPerDepVar = getValues kernelDefinition
        samplesPerDepVar = map (second drawSamples) randIterations
        drawSamples r    = ValuesPerDepVar $
            zipWith (getRandomSample obs dists r) depVars kernelsPerDepVar
    in V.fromList $ map (uncurry (InterpolationSample sett)) samplesPerDepVar

getRandomSample :: V.Vector Observation -> V.Vector IndepVarsDist
                -> DepVarsRands -> DepVarName -> KernelOneDepVar
                -> (DepVarName, Double)
getRandomSample obs dists depVarsRands depVar kernel = do
    let values   = VU.convert $ V.map (getDepVarsPos depVar) obs
        weights  = M.fromRows [VU.convert $ V.map (getWeight kernel) dists]
        random01 = lookupUnsafe depVarsRands depVar
    case V.head $ kas weights values of
        (_, _, _, _, Right distribution) -> (depVar, quantile distribution random01)
        (_, _, _, _, Left _)             -> (depVar, nan)

-- interpolation and search application
coreNormal :: Double -> Supplement -> [DepVarName]
           -> V.Vector Observation -> Permutation
           -> SearchResult
coreNormal spatDistUnitScaling coreSupplement
     depVars observations sett@(Permutation _ searchDepVarPos kernelDefinition _ _) =
    let (obs,dists)      = filterObs spatDistUnitScaling coreSupplement sett observations
        kernelsPerDepVar = getValues kernelDefinition
        searchPerDepVar  = case searchDepVarPos of
            Just (DepVarsPredPosDirect x)    -> Just <$> getValues x
            Just (DepVarsPredPosSearchObs x) -> Just <$> getValues ((_hyposDepVarsPos . _obsPos) x)
            Nothing                          -> replicate (length depVars) Nothing
        interpolPerDepVar = zipWith3 (interpol obs dists) depVars kernelsPerDepVar searchPerDepVar
    in SearchResult {
           _srPermutation   = sett
         , _srInterpolation = InterpolationResult interpolPerDepVar
         , _srLikelihood    =
             case mapMaybe getLogLikelihood interpolPerDepVar of
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

interpol :: V.Vector Observation -> V.Vector IndepVarsDist
         -> DepVarName -> KernelOneDepVar -> Maybe Double
         -> InterpolationResultOneDepVar
interpol obs dists depVar kernel maybeSearchValue = do
    let values  = VS.convert $ V.map (getDepVarsPos depVar) obs
        weights = M.fromRows [VS.convert $ V.map (getWeight kernel) dists]
    case V.head $ kas weights values of
        (neff, wvb, wv, mu, Right distribution) ->
            let lower  = quantile distribution 0.025
                median = mu -- quantile distribution 0.5
                upper  = quantile distribution 0.975
                logL   = fmap (logDensity distribution) maybeSearchValue -- log-likelihood
            in KAS depVar neff wvb wv True lower median upper logL
        (neff, wvb, wv, mu, Left _) -> case maybeSearchValue of
            Just _  -> KAS depVar neff wvb wv False (-inf) mu inf (Just (-inf))
            Nothing -> KAS depVar neff wvb wv False (-inf) mu inf Nothing

sumRows :: M.Matrix M.R -> M.Vector M.R
sumRows m = M.flatten $ m M.<> M.konst 1 (M.cols m, 1)

-- for this case application here weights is a vector and that simplifies the algorithm
-- I decided to keep the matrix version anyway, in case I want to refactor later
kas :: M.Matrix M.R -> M.Vector M.R -> V.Vector (Double, Double, Double, Double, Either String (LinearTransform StudentT))
kas weights y =
    V.zipWith6 (\neff wvb wv _mu _scale _dof -> (neff, wvb, wv, _mu, generalizedStudentT _mu _scale _dof))
        (V.convert totalWeight) (V.convert weightedVarBasic) (V.convert weightedVar)
        (V.convert mu) (V.convert scale) (V.convert dof)
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
      scale = M.cmap sqrt ((1 + 1/(totalWeight + 1)) * weightedVar)
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

computeWeight :: KernelShape -> SquaredWeightedDist -> Double
computeWeight SquaredExponential d = 1 / exp d
computeWeight Linear             d = 1 / (1 + sqrt d)

{-# LANGUAGE BangPatterns #-}
module LocEst.CoreAlgorithms where

import           LocEst.Distance
import           LocEst.Exceptions
import           LocEst.MathUtils
import           LocEst.Types

import           Data.Bifunctor          (second)
import           Data.List               (sortBy, zipWith4, transpose, singleton, zip4)
import           Data.Maybe              (catMaybes, mapMaybe, fromMaybe)
import qualified Data.Vector             as V
import qualified Data.Vector.Unboxed     as VU
import           Statistics.Distribution (logDensity, quantile)
import qualified Numeric.LinearAlgebra as M
import Statistics.Distribution.Transform (LinearTransform)
import Statistics.Distribution.StudentT (StudentT)
import LocEst.CLI.Utils

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


mymerge :: [[Interpolation]] -> [SearchResult2]
mymerge coreOut = 
    let interpolationResults = transpose coreOut
    in for interpolationResults (\is ->
        let depVars = map _iDepVar is
            interpolDepVars = map _iMedian is
        in SearchResult2 {
           _sr2Interpolation   = is
         , _sr2Search      = case mapMaybe _iSearch is of
                [] -> Nothing
                xs ->
                    let searchEntity = head $ map fst xs
                        searchDepVars = map (\x -> extractDepVarPos x searchEntity) depVars
                        depDist = euclideanDistance searchDepVars interpolDepVars
                    in Just Search {
                      _sSearchEntity = searchEntity
                    , _sLikelihoodsPerDepVar = map snd xs
                    , _sEuclideanDep  = depDist
                    , _sLogLikelihood = foldSum $ map snd xs -- sum, not product, because log-likelihood
                    , _sProbability   = Nothing
                    }
         })

interpolate :: Double -> CoreSupplement -> V.Vector Observation -> Maybe [DepVarsPredPos] -> [CorePermutation2] -> [Interpolation]
interpolate spatDistUnitScaling (CoreSupplement _ maybeSpatDistMap maybeTempSamples) observations maybeDepVarsPredGrid permutations =
         let indepVarsPosGrid = map _cas2IndepVarsPos permutations
             tempSampIt = head $ map _cas2TempSamplingIteration permutations
             crossIt    = head $ map _cas2CrossIteration permutations
             depVar     = head $ map _cas2DepVarName permutations
             kernel     = head $ map _cas2KernOneDepVar permutations
             y          = head $ map _cas2yOneDepVar permutations
             search     = head $ map _cas2SearchPosOneDepVar permutations
             weights    = pairwiseWeights spatDistUnitScaling maybeSpatDistMap maybeTempSamples tempSampIt observations (V.fromList indepVarsPosGrid) kernel
             res        = kas weights y search
             res2 = concat $  zipWith (\indepVarsPos (lower, median, upper, search) ->
                 case search of
                     Nothing -> singleton $ Interpolation tempSampIt crossIt indepVarsPos depVar kernel lower median upper Nothing
                     Just ms -> zipWith (\s l -> Interpolation tempSampIt crossIt indepVarsPos depVar kernel lower median upper (Just (s, l))) (fromMaybe [] maybeDepVarsPredGrid) (M.toList ms)
                 ) indepVarsPosGrid res
             --perm = zipWith4 (\i k t c -> CorePermutation i Nothing k t c) (V.toList indepVarsPosGrid) (repeat $ KernelDefinition [kernel]) (repeat tempSampIteration) (repeat crossSampIteration)
         in res2

sumRows :: M.Matrix M.R -> M.Vector M.R
sumRows m = M.flatten $ m M.<> M.konst 1 (M.cols m, 1)

kas :: M.Matrix M.R -> M.Vector M.R -> Maybe (M.Vector M.R) -> [(Double, Double, Double, Maybe (M.Vector M.R))]
kas weights y maybeSearchValues = zipWith3 queryDistribution (M.toList mu) (M.toList scale) (M.toList dof)
    where
      totalWeight = sumRows weights
      weightedAvg = M.flatten (weights <> M.asColumn y) / totalWeight
      values = M.fromRows $ replicate (M.rows weights) y
      weightedVarBasic = sumRows (weights * (values - M.asColumn weightedAvg) ** 2) / (totalWeight - 1)
      meanY = M.sumElements y / fromIntegral (M.size y)
      varSample = M.dot (y - M.scalar meanY) (y - M.scalar meanY) / fromIntegral (M.size y - 1)
      scaledS2 = (totalWeight - 1) * weightedVarBasic
      weightedVar = (scaledS2 + M.scalar varSample) / (totalWeight + 1)
      mu = weightedAvg
      scale = M.cmap sqrt ((1 + 1/totalWeight) * weightedVar)
      dof = totalWeight
      queryDistribution _mu _scale _dof = 
          case generalizedStudentT _mu _scale _dof of
              Right distribution ->
                  let lower  = quantile distribution 0.025
                      median = quantile distribution 0.5
                      upper  = quantile distribution 0.975
                      logL   = fmap (M.cmap (logDensity distribution)) maybeSearchValues -- log-likelihood
                  in (lower, median, upper, logL)
              Left e -> error $ show e

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

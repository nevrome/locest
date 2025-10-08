{-# LANGUAGE BangPatterns             #-}

module LocEst.CoreAlgorithms where

import           LocEst.Distance
import           LocEst.Exceptions
import           LocEst.MathUtils
import           LocEst.Types
import           LocEst.TypesFlat

import           Data.Bifunctor                    (second)
import           Data.List                         (sortBy)
import           Data.Maybe                        (catMaybes, mapMaybe)
import qualified Data.Vector                       as V
import qualified Data.Vector.Storable              as VS
import qualified Numeric.LinearAlgebra             as M
import           Statistics.Distribution           (logDensity, quantile)
import           Statistics.Distribution.StudentT  (StudentT)
import           Statistics.Distribution.Transform (LinearTransform)
import Statistics.Distribution.Normal (NormalDistribution)


interpolGPR :: V.Vector Observation -> V.Vector IndepVarsPos -> IndepVarsDistFlat -> IndepVarsDistFlat -> IndepVarsDistFlat
         -> Maybe (V.Vector DepVarsPredPos)
         -> DepVarName -> KernelOneDepVar 
         -> V.Vector SearchResultLong
interpolGPR obs grid distsObsGrid distsObsObs distsGridGrid maybeSearchValues depVar kernel =
    let values  = VS.convert $ V.map (getDepVarsPos depVar) obs
        weightsObsObs   = M.reshape (V.length obs)  $ computeWeightsFlat kernel distsObsObs
        weightsObsGrid  = M.reshape (V.length obs) $ computeWeightsFlat kernel distsObsGrid
        weightsGridGrid = M.reshape (V.length grid) $ computeWeightsFlat kernel distsGridGrid
        searchValues = fmap (V.map (getDepVarsPos2 depVar)) maybeSearchValues
    in V.map (search searchValues) $ gpr weightsObsObs weightsObsGrid weightsGridGrid values 0.1 0.00001
    where
        search searchValues (Right distribution) =
            let lower  = quantile distribution 0.025
                median = quantile distribution 0.5
                upper  = quantile distribution 0.975
                logL   = fmap (V.map $ logDensity distribution) searchValues -- log-likelihood
            in SSLKAS depVar lower median upper maybeSearchValues logL
        search searchValues (Left _) = case searchValues of
           Just x  -> SSLKAS depVar (-inf) nan inf maybeSearchValues (Just (V.replicate (V.length x) (-inf)))
           Nothing -> SSLKAS depVar (-inf) nan inf maybeSearchValues Nothing

gpr
  :: M.Matrix Double      -- ^ K_train (nTrain × nTrain)  -- obs–obs kernel
  -> M.Matrix Double      -- ^ K_cross (nPred × nTrain)    -- grid–obs kernel
  -> M.Matrix Double      -- ^ K_pred  (nPred × nPred)     -- grid–grid kernel
  -> M.Vector Double      -- ^ yVec    (nTrain)
  -> Double               -- ^ nugget noise term g
  -> Double               -- ^ jitter eps
  -> V.Vector (Either String NormalDistribution)
  -- -> (M.Vector Double, M.Matrix Double, M.Matrix Double) -- mean, covFull, covInterp
gpr kTrain0 kCross kPred0 yVec g eps =
    let nTrain = M.rows kTrain0
        nPred  = M.rows kPred0

        -- Training kernel + nugget
        kTrain = kTrain0 + M.scale g (M.ident nTrain)

        -- Solve instead of computing full inverse (this is better numerically than M.inv):
        ki     = M.inv kTrain
        -- GPR variance scale tau^2
        tau2hat = (yVec `M.dot` (ki M.#> yVec)) / fromIntegral nTrain

        -- Posterior mean
        mup = M.flatten $ kCross M.<> M.asColumn (ki M.#> yVec)

        -- Posterior cov (full)
        kPredFull = kPred0 + M.scale g (M.ident nPred)
        sigmaP    = M.scale tau2hat
                      (kPredFull - kCross M.<> (ki M.<> M.tr kCross))

        -- Posterior cov (interpolation variance)
        kPredNoNoise = kPred0 + M.scale eps (M.ident nPred)
        sigmaInt     = M.scale tau2hat
                        (kPredNoNoise - kCross M.<> (ki M.<> M.tr kCross))

    --in (mup, sigmaP, sigmaInt)
    in marginals mup sigmaP
    
-- Build vector of marginals
marginals :: M.Vector Double -> M.Matrix Double -> V.Vector (Either String NormalDistribution)
marginals meanVec covMat =
    let n = M.size meanVec
    in V.generate n $ \i ->
        let mu    = M.atIndex meanVec i
            var   = M.atIndex covMat (i,i) -- diagonal element
            std   = sqrt var
        in normal mu std

interpol :: V.Vector Observation -> IndepVarsDistFlat -> Maybe (V.Vector DepVarsPredPos)
         -> DepVarName -> KernelOneDepVar 
         -> V.Vector SearchResultLong
interpol obs distsObsGrid maybeSearchValues depVar kernel =
    let values  = VS.convert $ V.map (getDepVarsPos depVar) obs
        weights = M.reshape (V.length obs) $ computeWeightsFlat kernel distsObsGrid
        searchValues = fmap (V.map (getDepVarsPos2 depVar)) maybeSearchValues
    in V.map (search searchValues) $ kas weights values
    where
        search searchValues (Right distribution) =
            let lower  = quantile distribution 0.025
                median = quantile distribution 0.5
                upper  = quantile distribution 0.975
                logL   = fmap (V.map $ logDensity distribution) searchValues -- log-likelihood
            in SSLKAS depVar lower median upper maybeSearchValues logL
        search searchValues (Left _) = case searchValues of
           Just x  -> SSLKAS depVar (-inf) nan inf maybeSearchValues (Just (V.replicate (V.length x) (-inf)))
           Nothing -> SSLKAS depVar (-inf) nan inf maybeSearchValues Nothing

kas :: M.Matrix M.R -> M.Vector M.R -> V.Vector (Either String (LinearTransform StudentT))
kas weights y =
    V.zipWith3 (\_mu _scale _dof -> (generalizedStudentT _mu _scale _dof))
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

{-# INLINE sumRows #-}
sumRows :: M.Matrix M.R -> M.Vector M.R
sumRows m = M.flatten $ m M.<> M.konst 1 (M.cols m, 1)

{-# INLINE computeWeightsFlat #-}
computeWeightsFlat :: KernelOneDepVar -> IndepVarsDistFlat -> VS.Vector Double
computeWeightsFlat kernel (IndepVarsDistFlat tags payload stride) =
    let thetasList = getValues (_kodvLengths kernel)
        thetasU    = VS.fromList thetasList
    in VS.generate (VS.length tags) $ \i ->
        if not (tags `VS.unsafeIndex` i)
        then
            -- spat/temp case
            case thetasList of
              [spaceW, timeW] ->
                  let spatDist = payload `VS.unsafeIndex` (i*stride)
                      tempDist = payload `VS.unsafeIndex` (i*stride + 1)
                      ds2      = (spatDist / spaceW) ^ 2
                               + (tempDist / timeW) ^ 2
                  in computeWeight (_kodvShape kernel) ds2
              _ -> error "kernel mismatch for spat/temp case"
        else
            -- arbitrary case
            let base = i*stride
                !acc = go 0 0.0
                  where
                    go j !s
                      | j >= stride = s
                      | otherwise =
                          let d = payload `VS.unsafeIndex` (base+j)
                              t = thetasU `VS.unsafeIndex` j
                              x = d / t
                          in go (j+1) (s + x*x)
            in computeWeight (_kodvShape kernel) acc

{-# INLINE computeWeight #-}
computeWeight :: KernelShape -> SquaredWeightedDist -> Double
computeWeight SquaredExponential d = 1 / exp d
computeWeight Linear             d = 1 / (1 + sqrt d)

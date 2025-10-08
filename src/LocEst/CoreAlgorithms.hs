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
import           Statistics.Distribution           (logDensity, quantile, ContDistr)
import           Statistics.Distribution.StudentT  (StudentT)
import           Statistics.Distribution.Transform (LinearTransform)
import Statistics.Distribution.Normal (NormalDistribution)


gpr :: V.Vector Observation -> V.Vector IndepVarsPos -> IndepVarsDistFlat -> IndepVarsDistFlat -> IndepVarsDistFlat
         -> Maybe (V.Vector DepVarsPredPos)
         -> DepVarName -> KernelOneDepVar 
         -> V.Vector SearchResultLong
gpr obs grid distsObsGrid distsObsObs distsGridGrid maybeSearchValues depVar kernel =
    let values  = VS.convert $ V.map (getDepVarsPos depVar) obs
        weightsObsObs   = M.reshape (V.length obs)  $ computeWeightsFlat kernel distsObsObs
        weightsObsGrid  = M.reshape (V.length obs)  $ computeWeightsFlat kernel distsObsGrid
        weightsGridGrid = M.reshape (V.length grid) $ computeWeightsFlat kernel distsGridGrid
        resDistribution = gprCore weightsObsObs weightsObsGrid weightsGridGrid values 0.1
    in V.map (search depVar maybeSearchValues) resDistribution

kas :: V.Vector Observation -> IndepVarsDistFlat -> Maybe (V.Vector DepVarsPredPos)
         -> DepVarName -> KernelOneDepVar 
         -> V.Vector SearchResultLong
kas obs distsObsGrid maybeSearchValues depVar kernel =
    let values  = VS.convert $ V.map (getDepVarsPos depVar) obs
        weights = M.reshape (V.length obs) $ computeWeightsFlat kernel distsObsGrid
        resDistribution = kasCore weights values
    in V.map (search depVar maybeSearchValues) resDistribution

search :: ContDistr b => DepVarName -> Maybe (V.Vector DepVarsPredPos) -> Either String b -> SearchResultLong
search depVar maybeSearchValues (Right distribution) =
            let lower  = quantile distribution 0.025
                median = quantile distribution 0.5
                upper  = quantile distribution 0.975
                searchValues = fmap (V.map (getDepVarsPos2 depVar)) maybeSearchValues
                logL   = fmap (V.map $ logDensity distribution) searchValues -- log-likelihood
            in SSLKAS depVar lower median upper maybeSearchValues logL
search depVar maybeSearchValues (Left _) = case maybeSearchValues of
           Just x  -> SSLKAS depVar (-inf) nan inf maybeSearchValues (Just (V.replicate (V.length x) (-inf)))
           Nothing -> SSLKAS depVar (-inf) nan inf maybeSearchValues Nothing

gprCore ::
       M.Matrix Double -- obs–obs weights
    -> M.Matrix Double -- grid–obs weights
    -> M.Matrix Double -- grid–grid weights
    -> M.Vector Double -- y: measured values in dependent variable space
    -> Double          -- nugget noise term g
    -> V.Vector (Either String NormalDistribution)
  -- -> (M.Vector Double, M.Matrix Double, M.Matrix Double) -- mean, covFull, covInterp
gprCore d dx dxx y g =
        -- Number of observation points and grid points
    let nObs   = M.rows d
        nGrid  = M.rows dxx
        -- Step 1: Build training covariance matrix with nugget
        -- k = K_obs_obs + g * I_nObs
        k      = d + M.scale g (M.ident nObs)
        -- Step 2: Cholesky factorisation (kernel matrix is SPD: symmetric positive definite)
        -- trustSym tells hmatrix to treat 'k' as symmetric and avoid recomputing symmetry structure
        cholK  = M.chol (M.trustSym k)   -- Lower-triangular L such that k = L * L^T
        -- Step 3: Build right-hand side matrix for simultaneous solves
        -- First column = y (vector), remaining columns = dx^T (transposed grid–obs weights)
        rhs    = M.fromColumns (y : M.toColumns (M.tr dx))
        -- Step 4: Solve k * X = rhs using precomputed Cholesky factor
        -- This yields alpha (for y) and beta (for dx^T) without computing k^{-1} explicitly
        sol    = M.cholSolve cholK rhs
        -- Step 5: Extract alpha = k^{-1} * y   (first column of solution)
        alphaCol = M.takeColumns 1 sol
        -- Step 6: Extract beta = k^{-1} * dx^T (remaining columns of solution)
        beta     = M.dropColumns 1 sol
        -- Step 7: Variance scale tau^2-hat = (y^T * alpha) / nObs
        -- This favours shorter amplitude when data fits the prior better
        tau2hat  = (y `M.dot` M.flatten alphaCol) / fromIntegral nObs
        -- Step 8: Posterior mean predictions for grid: mu_p = dx * alpha
        mup      = M.flatten $ dx M.<> alphaCol
        -- Step 9: Posterior covariance for grid points (full)
        -- sigmaP = tau^2 [ (K_grid_grid + gI) - dx * beta ]
        dxxFull  = dxx + M.scale g (M.ident nGrid)
        sigmaP   = M.scale tau2hat (dxxFull - dx M.<> beta)
        -- Step 10: Interpolation variance (without nugget noise in grid-grid block)
        -- sigmaInt = tau^2 [ (K_grid_grid + epsI) - dx * beta ]
        -- dxxNoNoise = dxx + M.scale eps (M.ident nGrid)
        -- sigmaInt   = M.scale tau2hat (dxxNoNoise - dx M.<> beta)
    -- Step 11: Produce marginal NormalDistributions from mean and variance (full posterior)
    in marginals mup sigmaP
    --in (mup, sigmaP, sigmaInt)
    
marginals :: M.Vector Double -> M.Matrix Double -> V.Vector (Either String NormalDistribution)
marginals meanVec covMat =
    let diagCov = M.takeDiag covMat
        n = M.size meanVec
    in V.generate n $ \i ->
        let mu    = M.atIndex meanVec i
            var   = M.atIndex diagCov i
            std   = sqrt var
        in normal mu std

kasCore ::
       M.Matrix M.R
    -> M.Vector M.R
    -> V.Vector (Either String (LinearTransform StudentT))
kasCore weights y =
    V.zipWith3 generalizedStudentT (V.convert mu) (V.convert scale) (V.convert dof)
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

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
import qualified Data.Vector.Storable.Mutable as VSM
import Control.Monad (forM_)


gpr :: V.Vector Observation -> V.Vector IndepVarsPos -> IndepVarsDistFlat -> IndepVarsDistFlat -> IndepVarsDistFlat
         -> Maybe (V.Vector DepVarsPredPos)
         -> DepVarName -> KernelOneDepVar 
         -> V.Vector SearchResultLong
gpr obs grid distsObsGrid distsObsObs distsGridGrid maybeSearchValues depVar kernel =
    let values  = VS.convert $ V.map (getDepVarsPos depVar) obs
        weightsObsGrid  = M.reshape (V.length obs) $ computeWeightsFlat kernel distsObsGrid
        weightsObsObs   = expandHalfToMatrix (V.length obs) $ computeWeightsFlat kernel distsObsObs
        weightsGridGrid = expandHalfToMatrix (V.length grid) $ computeWeightsFlat kernel distsGridGrid
    -- in error $ show $ VS.take 100 $ VS.reverse $ M.flatten $ weightsGridGrid
        nugget = case _kodvNugget kernel of
            Just x -> x
            Nothing -> throwL "nugget parameter missing in kernel definition"
        resDistribution = gprCore weightsObsObs weightsObsGrid weightsGridGrid values nugget
    in V.map (search depVar maybeSearchValues) resDistribution

expandHalfToMatrix :: Int -> VS.Vector Double -> M.Matrix Double
expandHalfToMatrix n halfVec =
    let mv = VS.create $ do
          mvec <- VSM.new (n*n)
          let idx col row = col*n + row  -- column-major index
              idxHalf i j = i * (i+1) `div` 2 + j
          forM_ [0..n-1] $ \i -> 
            forM_ [0..i] $ \j -> do
              let v = halfVec VS.! idxHalf i j
              -- write (i,j) and (j,i)
              VSM.unsafeWrite mvec (idx j i) v
              VSM.unsafeWrite mvec (idx i j) v
          pure mvec
    in M.reshape n mv

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
            in SSL depVar lower median upper maybeSearchValues logL
search depVar maybeSearchValues (Left _) = case maybeSearchValues of
           Just x  -> SSL depVar (-inf) nan inf maybeSearchValues (Just (V.replicate (V.length x) (-inf)))
           Nothing -> SSL depVar (-inf) nan inf maybeSearchValues Nothing

gprCore ::
       M.Matrix Double -- obs–obs weights
    -> M.Matrix Double -- grid–obs weights
    -> M.Matrix Double -- grid–grid weights
    -> M.Vector Double -- y: measured values in dependent variable space
    -> Double          -- nugget noise term g
    -> V.Vector (Either String NormalDistribution)
  -- -> (M.Vector Double, M.Matrix Double, M.Matrix Double) -- mean, covFull, covInterp
gprCore d dx dxx y g =
    -- number of observations and grid points
    let nObs  = M.rows d
        nGrid = M.rows dxx
        -- training kernel + nugget
        k     = nearestPD 0.00001 $ d + M.scale g (M.ident nObs)
        -- Cholesky factorisation (SPD assumption)
        cholK = M.chol (M.trustSym k)
        -- RHS matrix: y and dx^T as columns
        rhs   = M.fromColumns (y : M.toColumns (M.tr dx))
        -- solve in one go, reusing factorisation
        sol   = M.cholSolve cholK rhs
        -- alpha = k^-1 y
        alphaCol = M.takeColumns 1 sol
        -- beta = k^-1 dx^T
        beta     = M.dropColumns 1 sol
        -- variance scale tau^2-hat
        tau2hat  = (y `M.dot` M.flatten alphaCol) / fromIntegral nObs
        -- posterior mean: dx * alpha
        mup      = M.flatten $ dx M.<> alphaCol
        -- full posterior covariance
        dxxFull  = dxx + M.scale g (M.ident nGrid)
        sigmaP   = M.scale tau2hat (dxxFull - dx M.<> beta)
        -- interpolation variance
        -- dxxNoNoise = dxx + M.scale eps (M.ident nGrid)
        -- sigmaInt   = M.scale tau2hat (dxxNoNoise - dx M.<> beta)
    in marginals mup sigmaP
    --in (mup, sigmaP, sigmaInt)

nearestPD :: Double -> M.Matrix Double -> M.Matrix Double
nearestPD eps mIn =
    let m       = M.sym mIn
        (evals, evecs) = M.eigSH m              -- evals :: Vector Double, evecs :: Matrix Double
        evalsPD = M.cmap (\x -> if x > eps then x else eps) evals
        mPD     = evecs <> M.diag evalsPD <> M.tr evecs
    in mPD

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
        -- multiplications are seemlingly faster in hot loops,
        -- so better compute the inverse first
        thetaInv   = VS.map (1/) thetasU
        n          = VS.length tags
        -- risky assumption: tag is always the same
        firstTag   = tags `VS.unsafeIndex` 0
        -- choose weight function based on kernel shape
        !weightFun = case _kodvShape kernel of
            SquaredExponential -> \ds2 -> 1 / exp ds2
            Linear             -> \ds2 -> 1 / (1 + sqrt ds2)
    in if not firstTag
         -- spat/temp case (stride == 2)
         then case thetasList of
                [spaceW, timeW] ->
                  let !spaceInv = 1 / spaceW
                      !timeInv  = 1 / timeW
                  in VS.generate n $ \i ->
                         let base = i * stride
                             sd   = payload `VS.unsafeIndex` base     * spaceInv
                             td   = payload `VS.unsafeIndex` (base+1) * timeInv
                         in weightFun (sd*sd + td*td)
                _ -> error "kernel mismatch: spat/temp case expects 2 thetas"
         -- arbitrary case (small stride loop)
         else VS.generate n $ \i ->
                let base = i * stride
                    !ds2 = let go !j !acc
                                 | j >= stride = acc
                                 | otherwise =
                                     let x = payload `VS.unsafeIndex` (base+j)
                                             * thetaInv `VS.unsafeIndex` j
                                   in go (j+1) (acc + x*x)
                           in go 0 0.0
                in weightFun ds2

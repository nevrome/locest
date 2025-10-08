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


interpol :: V.Vector Observation -> IndepVarsDistFlat -> Maybe (V.Vector DepVarsPredPos)
         -> DepVarName -> KernelOneDepVar 
         -> V.Vector SearchResultLong
interpol obs dists maybeSearchValues depVar kernel =
    let values  = VS.convert $ V.map (getDepVarsPos depVar) obs
        weights = M.reshape (V.length obs) $ computeWeightsFlat kernel dists
        searchValues = fmap (V.map (getDepVarsPos2 depVar)) maybeSearchValues
    in V.map (search searchValues) $ kas weights values
    where
        search searchValues (neff, wvb, wv, mu, Right distribution) =
            let lower  = quantile distribution 0.025
                median = mu -- quantile distribution 0.5
                upper  = quantile distribution 0.975
                logL   = fmap (V.map $ logDensity distribution) searchValues -- log-likelihood
            in SSLKAS depVar neff wvb wv True lower median upper maybeSearchValues logL
        search searchValues (neff, wvb, wv, mu, Left _) = case searchValues of
            Just x  -> SSLKAS depVar neff wvb wv False (-inf) mu inf maybeSearchValues (Just (V.replicate (V.length x) (-inf)))
            Nothing -> SSLKAS depVar neff wvb wv False (-inf) mu inf maybeSearchValues Nothing

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

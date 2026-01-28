{-# LANGUAGE BangPatterns #-}

module LocEst.CoreAlgorithms where

import           LocEst.Types
import           LocEst.TypesFlat
import           LocEst.Utils

import           Control.Monad                     (forM_)
import           Data.List                         (intercalate, sortOn)
import           Data.Ord                          (Down (..))
import qualified Data.Vector                       as V
import qualified Data.Vector.Storable              as VS
import qualified Data.Vector.Storable.Mutable      as VSM
import qualified Numeric.LinearAlgebra             as M
import           Statistics.Distribution           (ContDistr, logDensity,
                                                    quantile)
import           Statistics.Distribution.Normal    (NormalDistribution,
                                                    normalDistr)
import           Statistics.Distribution.StudentT  (StudentT,
                                                    studentTUnstandardized)
import           Statistics.Distribution.Transform (LinearTransform)

gpr :: V.Vector Observation
    -> V.Vector IndepVarsPos
    -> Maybe (V.Vector DepVarsPos)
    -> IndepVarsDistFlat
    -> IndepVarsDistFlat
    -- -> IndepVarsDistFlat
    -> Maybe (V.Vector DepVarsPredPos)
    -> Int
    -> DepVarName
    -> KernelOneDepVar
    -> V.Vector SearchResultLong
gpr obs _ -- grid
    maybeGridTrueDep distsObsGrid distsObsObs -- distsGridGrid
    maybeSearchValues topNObs depVar kernel =
    let values = VS.generate (V.length obs) $ \i -> getDepVarsPos depVar (obs V.! i)
        !weightsObsGrid  = M.reshape (V.length obs) $ computeWeightsFlat kernel distsObsGrid
        !weightsObsObs   = expandHalfToMatrix (V.length obs) $ computeWeightsFlat kernel distsObsObs
        -- !weightsGridGrid = expandHalfToMatrix (V.length grid) $ computeWeightsFlat kernel distsGridGrid
    -- in error $ show $ VS.take 100 $ VS.reverse $ M.flatten $ weightsGridGrid
        nugget = case _kodvNugget kernel of
            Just x  -> x
            Nothing -> throwL "nugget parameter missing in kernel definition"
        resDistribution = gprCore weightsObsObs weightsObsGrid Nothing values nugget
    in V.imap (\i ed ->
        let topObs   = if topNObs > 0
                       then Just $ topNObsIDs topNObs obs weightsObsGrid i
                       else Nothing
            mTrueDep = maybeGridTrueDep >>= (V.!? i)
        in seek depVar maybeSearchValues mTrueDep ed topObs
     ) resDistribution

expandHalfToMatrix :: Int -> VS.Vector Double -> M.Matrix Double
expandHalfToMatrix n halfVec =
    M.reshape n $ VS.create $ do
      mvec <- VSM.unsafeNew (n*n)
      let idx col row = col*n + row  -- column-major index
      forM_ [0..n-1] $ \i ->
        forM_ [0..i] $ \j -> do
          let v = halfVec `VS.unsafeIndex` idxHalf i j
          -- write (i,j) and (j,i)
          VSM.unsafeWrite mvec (idx j i) v
          VSM.unsafeWrite mvec (idx i j) v
      pure mvec

kas :: V.Vector Observation
    -> Maybe (V.Vector DepVarsPos)
    -> IndepVarsDistFlat
    -> Maybe (V.Vector DepVarsPredPos)
    -> Int
    -> DepVarName
    -> KernelOneDepVar
    -> V.Vector SearchResultLong
kas obs maybeGridTrueDep distsObsGrid maybeSearchValues topNObs depVar kernel =
    let values = VS.generate (V.length obs) $ \i -> getDepVarsPos depVar (obs V.! i)
        !weightsObsGrid = M.reshape (V.length obs) $ computeWeightsFlat kernel distsObsGrid
        resDistribution = kasCore weightsObsGrid values
    in V.imap (\i ed ->
        let topObs   = if topNObs > 0
                       then Just $ topNObsIDs topNObs obs weightsObsGrid i
                       else Nothing
            mTrueDep = maybeGridTrueDep >>= (V.!? i)
        in seek depVar maybeSearchValues mTrueDep ed topObs
     ) resDistribution

topNObsIDs
    :: Int
    -> V.Vector Observation
    -> M.Matrix Double   -- grid * obs
    -> Int               -- grid index
    -> String
topNObsIDs n obs weights gridIx =
    let row = [ (obs V.! j, weights `M.atIndex` (gridIx, j)) | j <- [0 .. V.length obs - 1] ]
    in intercalate ";" [ _obsID o | (o, _) <- take n (sortOn (Down . snd) row) ]

seek :: ContDistr b
    => DepVarName
    -> Maybe (V.Vector DepVarsPredPos)
    -> Maybe DepVarsPos
    -> Either String b
    -> Maybe String
    -> SearchResultLong
seek depVar maybeSearchValues maybeTrueDep (Right distribution) topObs =
    let lower  = quantile distribution 0.025
        median = quantile distribution 0.5
        upper  = quantile distribution 0.975
        logLTruth = do
            trueDep <- maybeTrueDep
            let trueVal = lookupUnsafe trueDep depVar
            pure (logDensity distribution trueVal)
        searchValues = fmap (V.map (getDepVarsPos2 depVar)) maybeSearchValues
        logL   = fmap (V.map $ logDensity distribution) searchValues -- log-likelihood
    in SRL depVar lower median upper maybeTrueDep logLTruth maybeSearchValues logL topObs
seek depVar maybeSearchValues maybeTrueDep (Left _) topObs =
    let logLTruth = maybeTrueDep *> Just (-inf)   -- if truth exists but dist failed, mark as -inf; else Nothing
        logLSearch = case maybeSearchValues of
                       Just x  -> Just (V.replicate (V.length x) (-inf))
                       Nothing -> Nothing
    in SRL depVar (-inf) nan inf maybeTrueDep logLTruth maybeSearchValues logLSearch topObs

gprCore
    :: M.Matrix Double -- obs–obs weights
    -> M.Matrix Double -- grid–obs weights
    -> Maybe (M.Matrix Double) -- grid–grid weights
    -> M.Vector Double -- y: measured values in dependent variable space
    -> Double          -- nugget noise term g
    -> V.Vector (Either String NormalDistribution)
  -- -> (M.Vector Double, M.Matrix Double, M.Matrix Double) -- mean, covFull, covInterp
gprCore d dx dxx y g =
    -- number of observations and grid points
    let nObs  = M.rows d
        -- training kernel + nugget
       --k     = nearestPD 0.00001 $ d + M.scale g (M.ident nObs)
        k     = d + M.scale g (M.ident nObs)
        -- Cholesky factorisation (SPD assumption)
        cholK = M.chol (M.trustSym k)
        -- alpha = k^-1 y
        alpha = M.cholSolve cholK (M.asColumn y)
        -- beta = k^-1 dx^T
        beta  = M.cholSolve cholK (M.tr dx)
        -- variance scale tau^2-hat
        tau2hat  = (y `M.dot` M.flatten alpha) / fromIntegral nObs
        -- posterior mean: dx * alpha
        mup      = M.flatten $ dx M.<> alpha
        -- sigmaP diagonal only:
        dxxDiag   = case dxx of
                        Just x  -> M.takeDiag x
                        Nothing -> VS.replicate (M.rows dx) 1.0
        betaT     = M.tr beta
        diagTerm  = sumRows (dx * betaT)
        sigmaDiag = VS.zipWith (\dxxi diTerm -> tau2hat * (dxxi + g - diTerm)) dxxDiag diagTerm
        -- full posterior covariance
        -- nGrid = M.rows dxx
        -- dxxFull  = dxx + M.scale g (M.ident nGrid)
        -- sigmaP   = M.scale tau2hat (dxxFull - dx M.<> beta)
        -- interpolation variance
        -- dxxNoNoise = dxx + M.scale eps (M.ident nGrid)
        -- sigmaInt   = M.scale tau2hat (dxxNoNoise - dx M.<> beta)
    in marginalsFromDiag mup sigmaDiag
    --in marginals mup sigmaP
    --in (mup, sigmaP, sigmaInt)

{-# INLINE sumRows #-}
sumRows :: M.Matrix M.R -> M.Vector M.R
sumRows m = M.flatten $ m M.<> M.konst 1 (M.cols m, 1)

marginalsFromDiag :: M.Vector Double -> VS.Vector Double -> V.Vector (Either String NormalDistribution)
marginalsFromDiag meanVec varVec =
    let n = M.size meanVec
    in V.generate n $ \i ->
        let mu  = M.atIndex meanVec i
            var = varVec VS.! i
            std = sqrt var
        in normal mu std

normal :: Double -> Double -> Either String NormalDistribution
normal mu std
    | isNaN std = Left "sigma is NaN"
    | std <= 0  = Left "sigma must be > 0"
    | otherwise = Right $ normalDistr mu std

-- make positive-definite with the sledgehammer
-- nearestPD :: Double -> M.Matrix Double -> M.Matrix Double
-- nearestPD eps mIn =
--     let m       = M.sym mIn
--         (evals, evecs) = M.eigSH m
--         evalsPD = M.cmap (\x -> if x > eps then x else eps) evals
--         mPD     = evecs <> M.diag evalsPD <> M.tr evecs
--     in mPD

-- marginals :: M.Vector Double -> M.Matrix Double -> V.Vector (Either String NormalDistribution)
-- marginals meanVec covMat =
--     let diagCov = M.takeDiag covMat
--         n = M.size meanVec
--     in V.generate n $ \i ->
--         let mu    = M.atIndex meanVec i
--             var   = M.atIndex diagCov i
--             std   = sqrt var
--         in normal mu std

kasCore
    :: M.Matrix M.R
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

-- mapping Mathematica's StudentTDistribution interface to the interface in the
-- Haskell statistics package
generalizedStudentT :: Double -> Double -> Double -> Either String (LinearTransform StudentT)
generalizedStudentT mu scale dof
    | isNaN scale = Left "sigma is NaN"
    | scale <= 0  = Left "sigma must be > 0"
    | dof   <= 0  = Left "degree of freedoms must be > 0"
    | otherwise   = Right $ studentTUnstandardized dof mu scale

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
            SquaredExponential -> exp . negate -- \ds2 -> 1 / exp ds2
            Linear             -> \ds2 -> 1 / (1 + sqrt ds2)
            Exponential        -> exp . negate . sqrt -- \ds2 -> 1 / exp (sqrt ds2
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

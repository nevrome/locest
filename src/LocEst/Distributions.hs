{-# LANGUAGE DeriveGeneric #-}

module LocEst.Distributions where

import           LocEst.Utils

import           Control.DeepSeq
import           GHC.Generics                     (Generic)
import           Statistics.Distribution          (cumulative, logDensity,
                                                   quantile)
import           Statistics.Distribution.Normal   (normalDistr)
import           Statistics.Distribution.StudentT (studentTUnstandardized)

normal :: Double -> Double -> Either String PredDist
normal mu std
    | isNaN std = Left "sigma is NaN"
    | std <= 0  = Left "sigma must be > 0"
    | otherwise = Right $ PredNormal mu std

generalizedStudentT :: Double -> Double -> Double -> Either String PredDist
generalizedStudentT mu scale dof
    | isNaN scale = Left "sigma is NaN"
    | scale <= 0  = Left "sigma must be > 0"
    | dof   <= 0  = Left "degree of freedoms must be > 0"
    | otherwise   = Right $ PredStudentT dof mu scale

-- | A data type for the parameters of statistical distributions
data PredDist
    = PredNormal !Double !Double -- mean, sd
    | PredStudentT !Double !Double !Double -- dof, location, scale
    | PredMixture [PredDist] -- equally weighted mixture, used after temporal marginalisation
    deriving (Eq, Show, Generic)

instance NFData PredDist

predQuantile :: PredDist -> Double -> Double
predQuantile (PredNormal mu sd) p =
    quantile (normalDistr mu sd) p
predQuantile (PredStudentT dof mu scale) p =
    quantile (studentTUnstandardized dof mu scale) p
predQuantile (PredMixture ds) p =
    mixtureQuantile ds p

predCDF :: PredDist -> Double -> Double
predCDF (PredNormal mu sd) x =
    cumulative (normalDistr mu sd) x
predCDF (PredStudentT dof mu scale) x =
    cumulative (studentTUnstandardized dof mu scale) x
predCDF (PredMixture ds) x =
    case ds of
      [] -> nan
      _  -> sum [predCDF d x | d <- ds] / fromIntegral (length ds)

predLogDensity :: PredDist -> Double -> Double
predLogDensity (PredNormal mu sd) x =
    logDensity (normalDistr mu sd) x
predLogDensity (PredStudentT dof mu scale) x =
    logDensity (studentTUnstandardized dof mu scale) x
predLogDensity (PredMixture ds) x =
    logMeanExp [predLogDensity d x | d <- ds]

logMeanExp :: [Double] -> Double
logMeanExp [] = nan
logMeanExp xs =
    let m = maximum xs
    in if isInfinite m && m < 0
       then -inf
       else m + log (sum [exp (x - m) | x <- xs])
              - log (fromIntegral (length xs))

mixtureQuantile :: [PredDist] -> Double -> Double
mixtureQuantile [] _ = nan
mixtureQuantile ds p = bisect 100 lo0 hi0
  where
    eps = 1e-12
    loStart = minimum [predQuantile d eps | d <- ds]
    hiStart = maximum [predQuantile d (1 - eps) | d <- ds]
    mixtureCDF x = sum [predCDF d x | d <- ds] / fromIntegral (length ds)
    expand lo hi
        | mixtureCDF lo <= p && mixtureCDF hi >= p = (lo, hi)
        | otherwise =
            let w = hi - lo
            in expand (lo - w) (hi + w)
    (lo0, hi0) = expand loStart hiStart
    bisect :: Int -> Double -> Double -> Double
    bisect 0 lo hi = 0.5 * (lo + hi)
    bisect n lo hi =
        let mid = 0.5 * (lo + hi)
        in if mixtureCDF mid >= p
           then bisect (n - 1) lo mid
           else bisect (n - 1) mid hi

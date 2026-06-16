{-# LANGUAGE DeriveGeneric #-}

module LocEst.Distributions where

import           Control.DeepSeq
import           GHC.Generics                     (Generic)
import           Statistics.Distribution          (cumulative, logDensity,
                                                   quantile)
import           Statistics.Distribution.Normal   (normalDistr)
import           Statistics.Distribution.StudentT (studentTUnstandardized)

-- | A data type for the parameters of statistical distributions
data PredDist
    = PredNormal !Double !Double -- mean, sd
    | PredStudentT !Double !Double !Double -- dof, location, scale
    deriving (Eq, Show, Generic)

instance NFData PredDist

-- smart constructors
makePredNormal :: Double -> Double -> Either String PredDist
makePredNormal mu sd
    | isNaN mu || isNaN sd = Left "normal has NaN"
    | sd <= 0              = Left "normal sd must be > 0"
    | otherwise            = Right $ PredNormal mu sd

makePredStudentT :: Double -> Double -> Double -> Either String PredDist
makePredStudentT mu scale dof
    | isNaN dof || isNaN mu || isNaN scale = Left "student-t has NaN parameter"
    | scale <= 0 = Left "student-t scale must be > 0"
    | dof <= 1 = Left "student-t mean is undefined for dof <= 1"
    | dof <= 2 = Left "student-t variance is infinite for dof <= 2"
    | otherwise = Right $ PredStudentT dof mu scale

-- query distributions
predQuantile :: PredDist -> Double -> Double
predQuantile (PredNormal mu sd) p = quantile (normalDistr mu sd) p
predQuantile (PredStudentT dof mu scale) p = quantile (studentTUnstandardized dof mu scale) p

predCDF :: PredDist -> Double -> Double
predCDF (PredNormal mu sd) x = cumulative (normalDistr mu sd) x
predCDF (PredStudentT dof mu scale) x = cumulative (studentTUnstandardized dof mu scale) x

predLogDensity :: PredDist -> Double -> Double
predLogDensity (PredNormal mu sd) x = logDensity (normalDistr mu sd) x
predLogDensity (PredStudentT dof mu scale) x = logDensity (studentTUnstandardized dof mu scale) x

predMoments :: PredDist -> (Double, Double)
predMoments (PredNormal mu sd)          = (mu, sd * sd)
predMoments (PredStudentT dof mu scale) = (mu, scale * scale * dof / (dof - 2))

-- moment-matched mixture approximation:
-- given n predictive distributions, this returns a single
-- normal distribution whose mean and variance match the equally weighted mixture
mix :: [Either String PredDist] -> Either String PredDist
mix [] = Left "mix: empty"
mix [Right x] = Right x
mix xs = do
    moments <- traverse (fmap predMoments) xs
    let n = fromIntegral (length moments)
        mean = sum [mu | (mu, _) <- moments] / n
        -- law of total variance
        var = sum [v + (mu - mean)**2 | (mu, v) <- moments] / n
        sd = sqrt var
    either (const (Left "mix: can't mix")) Right (makePredNormal mean sd)

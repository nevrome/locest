module LocEst.MathUtils where

import           Data.List                         (foldl')
import qualified Data.Vector.Unboxed               as VU
import           Statistics.Distribution.StudentT  (StudentT,
                                                    studentTUnstandardized)
import           Statistics.Distribution.Transform (LinearTransform)

infinity :: Fractional a => a
infinity = 1/0

nan :: Fractional a => a
nan = 0/0

-- should be slightly faster than sum, because sum is implemented with the lazy foldl
-- GHC does this optimization automatically, but only with -O2
foldSum :: [Double] -> Double
foldSum = foldl' (+) 0

foldProduct :: [Double] -> Double
foldProduct = foldl' (*) 1

-- some of the following functions are available in independent versions and some also
-- in versions with a _ suffix that try to avoid re-computation of shared values
avg :: [Double] -> Double
avg xs = foldSum xs / fromIntegral (length xs)

avg_ :: Double -> VU.Vector Double -> Double
avg_ xslength xs = VU.sum xs / xslength

-- with Bessel's correction
-- https://en.wikipedia.org/wiki/Variance#Unbiased_sample_variance
varSample :: [Double] -> Double
varSample xs = (1/(n-1)) * foldl' (\o x -> o + (x - mean)**2) 0 xs
    where
        mean = avg xs
        n = fromIntegral $ length xs

varSample_ :: Double -> VU.Vector Double -> Double
varSample_ xslength xs = (1/(n-1)) * VU.foldl' (\o x -> o + (x - mean)**2) 0 xs
    where
        mean = avg_ xslength xs
        n = xslength

sdSample :: [Double] -> Double
sdSample xs = sqrt $ varSample xs

weightedAvg :: [Double] -> [Double] -> Double
weightedAvg values weights =
    foldl' (\o (v,w) -> o + v * w) 0 (zip values weights) / foldSum weights

weightedAvg_ :: Double -> VU.Vector Double -> VU.Vector Double -> Double
weightedAvg_ totalWeight values weights =
    VU.foldl' (\o (v,w) -> o + v * w) 0 (VU.zip values weights) / totalWeight

weightedVar :: [Double] -> [Double] -> Double
weightedVar values weights =
    (nu0 * sigma02 + scaledS2) / (nu0 + neff)
    where
        scaledS2 = if neff < 1
                   then 0
                   else (neff - 1) * s2
        s2 = foldl' (\o (v,w) -> o + w * ((v - weightedMean) ** 2)) 0 (zip values weights)
        weightedMean = weightedAvg values weights
        neff = totalWeight
        totalWeight = foldSum weights
        nu0 = 2
        sigma02 = varSample values

weightedVar_ :: Double -> Double -> VU.Vector Double -> VU.Vector Double -> Double
weightedVar_ totalWeight weightedMean values weights =
    numerator / neff
    where
        numerator = VU.foldl' (\o (v,w) -> o + w * ((v - weightedMean) ** 2)) 0 (VU.zip values weights)
        neff = totalWeight

weightedSD :: [Double] -> [Double] -> Double
weightedSD values weights =
    sqrt $ weightedVar values weights

weightedSEM :: [Double] -> [Double] -> Double
weightedSEM values weights =
    sqrt (weightedVar values weights / neff)
    where
        neff = totalWeight
        totalWeight = foldSum weights

posteriorMu :: [Double] -> [Double] ->  Either String (LinearTransform StudentT)
posteriorMu values weights = generalizedStudentT mu scale dof
    where
        mu = weightedAvg values weights
        scale = weightedSD values weights / sqrt neff
        dof = neff1
        neff1 = neff - 1
        neff = totalWeight + 1
        totalWeight = foldSum weights

posteriorPredictive :: [Double] -> [Double] -> Either String (LinearTransform StudentT)
posteriorPredictive values weights = generalizedStudentT mu scale dof
    where
        mu = weightedAvg values weights
        scale = sqrt ((1 + 1/neff) * weightedVar values weights)
        dof = neff - 1
        neff = foldSum weights + 1

posteriorPredictive_ :: Double -> Double -> Double -> Either String (LinearTransform StudentT)
posteriorPredictive_ totalWeight weightedM weightedV = generalizedStudentT mu scale dof
    where
        mu = weightedM
        scale = sqrt ((1 + 1/neff) * weightedV)
        dof = neff - 1
        neff = totalWeight + 1

-- mapping Mathematica's StudentTDistribution interface to the interface in the
-- Haskell statistics package
generalizedStudentT :: Double -> Double -> Double -> Either String (LinearTransform StudentT)
generalizedStudentT mu scale dof
    | scale <= 0 = Left "sigma must be > 0"
    | dof   <= 0 = Left "degree of freedoms must be > 0"
    | otherwise  = Right $ studentTUnstandardized dof mu scale

-- | get the density of student's-t distribution at a point x
-- dof: number of degrees of freedom
-- requires import Numeric.SpecFunctions (logBeta) from math-functions
--dt :: Double -> Double -> Double
--dt dof x =
--    let logDensityUnscaled = log (dof / (dof + x*x)) * (0.5 * (1 + dof)) - logBeta 0.5 (0.5 * dof)
--    in exp logDensityUnscaled / sqrt dof
    -- alternative implemenation with the statistics package:
    -- import Statistics.Distribution.StudentT (studentT)
    -- density (studentT dof) x

-- | get the density of a normal distribution at a point x
dnorm :: Double -> Double -> Double -> Double
dnorm mu sigma x =
    let a = recip (sqrt (2 * pi * sigma2)) -- recip: returns 1 / argument
        b = exp (-c2 / (2 * sigma2))
        c = x - mu
        c2 = c * c
        sigma2 = sigma * sigma
    in a*b

-- this applies only for basic covariance matrices (only values on the diagonals)
-- was mostly written by ChatGPT and only tested by comparing to mvtnorm::dmvnorm in R
dnormMulti :: [Double] -> [Double] -> [Double] -> Double
dnormMulti mus sigmas positions = constantFactor * exp (-0.5 * exponentTerm)
  where
    constantFactor = 1 / sqrt (product $ map (* (2 * pi)) sigmasSq)
    sigmasSq = map (** 2) sigmas
    exponentTerm = foldl' (\acc (mu, sigma, x) -> acc + ((x - mu) / sigma) ** 2) 0 terms
    terms = zip3 mus sigmas positions

-- https://stackoverflow.com/questions/32978290/haskell-numerical-integration-via-trapezoidal-rule-results-in-wrong-sign
integrate :: Double -> (Double -> Double) -> Double -> Double -> Double
integrate steps f start stop =
    h / 2 * (f start + f stop + 2 * partial_sum)
    where
        h = (stop - start) / steps
        myPoints = points (steps-1) h
        partial_sum = foldl' (\o x -> o + f (x + start)) 0 myPoints
        points  :: Double -> Double -> [Double]
        points x1 x2
            | x1 <= 0 = []
            | otherwise = (x1*x2) : points (x1-1) x2

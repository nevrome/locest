module LocEst.MathUtils where

import           Data.List (foldl')
import Statistics.Distribution.StudentT (studentTUnstandardized, StudentT)
import Statistics.Distribution.Transform (LinearTransform)

-- should be slightly faster than sum, because sum is implemented with the lazy foldl
-- GHC does this optimization automatically, but only with -O2
foldSum :: [Double] -> Double
foldSum = foldl' (+) 0

avg :: [Double] -> Double
avg xs = foldSum xs / fromIntegral (length xs)

sd :: [Double] -> Double
sd xs = sqrt . avg . map ((**2) . (-) (avg xs)) $ xs

-- the following functions are all independent now - they could be
-- optimized by avoiding re-computation of shared values
weightedAvg :: [Double] -> [Double] -> Double
weightedAvg values weights =
    foldl' (\o (v,w) -> o + v * w) 0 (zip values weights) / foldSum weights

weightedVar :: [Double] -> [Double] -> Double
weightedVar values weights =
    numerator / neff1
    where
        numerator = foldl' (\o (v,w) -> o + w * ((v - weightedMean) ** 2)) 0 (zip values weights)
        weightedMean = weightedAvg values weights
        neff1 = totalWeight - 1
        totalWeight = foldSum weights

weightedSD :: [Double] -> [Double] -> Double
weightedSD values weights =
    sqrt $ weightedVar values weights

weightedSEM :: [Double] -> [Double] -> Double
weightedSEM values weights =
    sqrt (weightedVar values weights / neff1)
    where
        neff1 = totalWeight - 1
        totalWeight = foldSum weights

posteriorMu :: [Double] -> [Double] -> LinearTransform StudentT
posteriorMu values weights =
    generalizedStudentT mu scale dof
    where
        mu = weightedAvg values weights
        scale = weightedSD values weights / sqrt neff
        dof = neff1
        neff1 = neff - 1
        neff = totalWeight
        totalWeight = foldSum weights

posteriorPredictive :: [Double] -> [Double] -> Either String (LinearTransform StudentT)
posteriorPredictive values weights =
    if scale > 0
    then Right $ generalizedStudentT mu scale dof
    else Left "sigma must be > 0"
    where
        mu = weightedAvg values weights
        scale = sqrt ((1 + 1/n) * weightedVar values weights)
        n = fromIntegral $ length values
        dof = neff1
        neff1 = totalWeight - 1
        totalWeight = foldSum weights

-- mapping Mathematica's StudentTDistribution interface to the interface in the
-- Haskell statistics package
generalizedStudentT :: Double -> Double -> Double -> LinearTransform StudentT
generalizedStudentT mu scale dof = studentTUnstandardized dof mu scale

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
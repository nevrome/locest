module LocEst.Math.Basics where

import           Data.List (foldl')

-- https://statss.stackexchange.com/questions/6534/how-do-i-calculate-a-weighted-standard-deviation-in-excel
-- http://seismo.berkeley.edu/~kirchner/Toolkits/Toolkit_12.pdf -> Case I !
weightedSEM :: [Double] -> [Double] -> Double
weightedSEM values weights =
    sqrt (weightedVar values weights / effn)
    where
        totalWeight = sum weights
        effn = neff totalWeight weights

neff :: Double -> [Double] -> Double
neff totalWeight weights = (totalWeight ** 2) / sum (map (** 2) weights)

weightedSD :: [Double] -> [Double] -> Double
weightedSD values weights = sqrt (weightedVar values weights)

weightedVar :: [Double] -> [Double] -> Double
weightedVar values weights =
    (numerator / totalWeight) * (effn / (effn - 1))
    where
        numerator = sum $ zipWith (\v w -> w * ((v - weightedMean) ** 2)) values weights
        weightedMean = weightedAvg values weights
        totalWeight = sum weights
        effn = neff totalWeight weights

weightedAvg :: [Double] -> [Double] -> Double
weightedAvg values weights =
    let sumWeightedVals = foldl' (\o (v,w) -> o + v * w) 0 $ zip values weights
    in sumWeightedVals / sum weights

sd :: [Double] -> Double
sd xs = sqrt . avg . map ((**2) . (-) (avg xs)) $ xs

avg :: [Double] -> Double
avg xs = sum xs / fromIntegral (length xs)

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

module LocEst.Math.Basics where

import           Data.List (foldl')

--integrateFaster :: [Double] -> (Double -> Double) -> Double -> Double -> Double

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
    let a = recip (sqrt (2 * pi * sigma2))
        b = exp (-c2 / (2 * sigma2))
        c = x - mu
        c2 = c * c
        sigma2 = sigma * sigma
    in a*b

weightedAvg :: [Double] -> [Double] -> Double
weightedAvg weights values =
    let sumWeightedVals = foldl' (\o (w,v) -> o + w * v) 0 $ zip weights values
    in sumWeightedVals / sum weights

avg :: [Double] -> Double
avg xs = sum xs / fromIntegral (length xs)

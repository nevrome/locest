module LocEst.Math where

import Data.List (foldl')

-- https://stackoverflow.com/questions/32978290/haskell-numerical-integration-via-trapezoidal-rule-results-in-wrong-sign
integrate :: (Double -> Double) -> Double -> Double -> Double
integrate f a b =
    h / 2 * (f a + f b + 2 * partial_sum)
    where
        h = (b - a) / 100
        most_parts  = map f (pointsWithOffset (100-1) h a)
        partial_sum = sum most_parts

pointsWithOffset :: Double -> Double -> Double -> [Double]
pointsWithOffset x1 x2 offset = map (+offset) (points x1 x2)

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
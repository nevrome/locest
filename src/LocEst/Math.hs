module LocEst.Math where

import Data.List (foldl')

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
    let sumWeights = sum weights
        sumWeightedVals = foldl' (\o (w,v) -> o + w * v) 0 $ zip weights values
    in sumWeightedVals / sumWeights

avg :: [Double] -> Double
avg xs = sum xs / fromIntegral (length xs)
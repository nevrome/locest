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

avg :: [Double] -> Double
avg xs = let sum_ = foldl' (+) 0 xs
         in sum_ / fromIntegral (length xs)
module LocEst.MathUtils (
       inf,
       nan,
       foldSum,
       avg,
       generalizedStudentT
    ) where

import           Data.List                         (foldl')
import           Statistics.Distribution.StudentT  (StudentT,
                                                    studentTUnstandardized)
import           Statistics.Distribution.Transform (LinearTransform)

inf :: Fractional a => a
inf = 1/0

nan :: Fractional a => a
nan = 0/0

-- should be slightly faster than sum, because sum is implemented with the lazy foldl
-- GHC does this optimization automatically, but only with -O2
foldSum :: [Double] -> Double
foldSum = foldl' (+) 0

avg :: [Double] -> Double
avg xs = foldSum xs / fromIntegral (length xs)

-- mapping Mathematica's StudentTDistribution interface to the interface in the
-- Haskell statistics package
generalizedStudentT :: Double -> Double -> Double -> Either String (LinearTransform StudentT)
generalizedStudentT mu scale dof
    | isNaN scale = Left "sigma is NaN"
    | scale <= 0  = Left "sigma must be > 0"
    | dof   <= 0  = Left "degree of freedoms must be > 0"
    | otherwise   = Right $ studentTUnstandardized dof mu scale

-- the following code is not used any more, but remains here as useful documentation of the kas algorithm

-- with Bessel's correction
-- https://en.wikipedia.org/wiki/Variance#Unbiased_sample_variance
-- varSample :: [Double] -> Double
-- varSample xs = foldl' (\o x -> o + (x - mean)**2) 0 xs / (n-1)
--     where
--         mean = avg xs
--         n = fromIntegral $ length xs

-- sdSample :: [Double] -> Double
-- sdSample xs = sqrt $ varSample xs

-- weightedAvg :: [Double] -> [Double] -> Double
-- weightedAvg values weights =
--     foldl' (\o (v,w) -> o + v * w) 0 (zip values weights) / foldSum weights

-- weightedVarBasic :: [Double] -> [Double] -> Double
-- weightedVarBasic values weights =
--     foldl' (\o (v,w) -> o + w * ((v - weightedMean) ** 2)) 0 (zip values weights) / (neff-1)
--     where
--         weightedMean = weightedAvg values weights
--         neff = foldSum weights

-- weightedVar :: [Double] -> [Double] -> Double
-- weightedVar values weights =
--     (nu0 * sigma02 + scaledS2) / (nu0 + neff)
--     where
--         scaledS2 = (neff - 1) * s2
--         s2 = weightedVarBasic values weights
--         neff = foldSum weights
--         nu0 = 1
--         sigma02 = varSample values

-- weightedSD :: [Double] -> [Double] -> Double
-- weightedSD values weights =
--     sqrt $ weightedVar values weights

-- weightedSEM :: [Double] -> [Double] -> Double
-- weightedSEM values weights =
--     sqrt (weightedVar values weights / neff)
--     where
--         neff = totalWeight
--         totalWeight = foldSum weights

-- posteriorMu :: [Double] -> [Double] ->  Either String (LinearTransform StudentT)
-- posteriorMu values weights = generalizedStudentT mu scale dof
--     where
--         mu = weightedAvg values weights
--         scale = weightedSD values weights / sqrt neff
--         dof = neff1
--         neff1 = neff - 1
--         neff = totalWeight + 1
--         totalWeight = foldSum weights

-- posteriorPredictive :: [Double] -> [Double] -> Either String (LinearTransform StudentT)
-- posteriorPredictive values weights = generalizedStudentT mu scale dof
--     where
--         mu = weightedAvg values weights
--         scale = sqrt ((1 + 1/neff) * weightedVar values weights)
--         dof = neff - 1
--         neff = foldSum weights + 1

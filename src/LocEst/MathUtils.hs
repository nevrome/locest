module LocEst.MathUtils (
       inf,
       nan,
       foldSum,
       avg,
       generalizedStudentT,
       normal
    ) where

import           Data.List                         (foldl')
import           Statistics.Distribution.Normal    (NormalDistribution,
                                                    normalDistr)
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

normal :: Double -> Double -> Either String NormalDistribution
normal mu std
    | isNaN std = Left "sigma is NaN"
    | std <= 0  = Left "sigma must be > 0"
    | otherwise = Right $ normalDistr mu std


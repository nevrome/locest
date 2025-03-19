import LocEst.MathUtils

import Numeric.LinearAlgebra
import           Statistics.Distribution.StudentT  (StudentT,
                                                    studentTUnstandardized)
import           Statistics.Distribution.Transform (LinearTransform)
import           Statistics.Distribution (logDensity, quantile)
import System.Environment (getArgs)
import Control.Monad (when)
import System.Exit (exitFailure)
import Prelude  hiding ((<>))
import Data.List (singleton)

main :: IO ()
main = do
    args <- getArgs  -- Get the list of arguments
    when (length args /= 3) $ do
        putStrLn "Usage: program <x_value> <y_value> <t_value>"
        exitFailure
    -- Convert the arguments to the appropriate types
    let x = (read $ args !! 1) :: [Double]
        y = (read $ args !! 2) :: [Double]
        t = (read $ args !! 3) :: Double
        
    let xx = undefined
    
    let (lower, median, upper) = kernelAverageSmoothing x y t xx
    putStrLn $ "Lower: " ++ show lower
    putStrLn $ "Median: " ++ show median
    putStrLn $ "Upper: " ++ show upper

-- calculate square of Euclidean norm
sqEuclideanNorm :: Matrix R -> Matrix R -> R
sqEuclideanNorm a b = sumElements $ (a - b) ** 2

-- calculate the distance matrix
distanceMatrix :: Matrix R -> Matrix R -> Matrix R
distanceMatrix m1 m2 =
  matrix (cols m1) [ sqEuclideanNorm (m1 ? [i]) (m2 ? [j]) | i <- [0 .. (cols m1 - 1)], j <- [0 .. (cols m2 - 1)] ]

rowSums :: Matrix R -> Vector R
rowSums m = vector $ map sumElements (toRows m)

vectorMean :: Vector R -> R
vectorMean v = sumElements v / fromIntegral (size v)

-- Function for computing kernel average smoothing
kernelAverageSmoothing :: [Double] -> [Double] -> Double -> [Double] -> (Vector R, Vector R, Vector R)
kernelAverageSmoothing x y t xx = do
    --distribution <- forM [1 .. rows xx] $ \i ->
    --  generalizedStudentT (mu ! i) (scale ! i) dof
    --let lower  = quantile distribution 0.025
    --    median = quantile distribution 0.5
    --    upper  = quantile distribution 0.975
    (mu, scale, dof)
    where
      values = fromRows $ replicate (length xx) (fromList y)
      dists = distanceMatrix (fromColumns $ singleton $ fromList xx) (fromColumns $ singleton $ fromList x) -- + scalar 0.01
      weights = cmap (exp . negate) (dists / scalar t ^ 2)
      totalWeight = rowSums weights
      weightedAvg = rowSums (values * weights) / totalWeight
      weightedVarBasic = rowSums (weights * (values - asColumn weightedAvg) ^ 2) / (totalWeight - 1)
      scaledS2 = (totalWeight - 1) * weightedVarBasic
      varSample = sumElements (fromList (map (\x-> (x - avg y)^2) y)) / (fromIntegral (length y) - 1)
      weightedVar = cmap (+ varSample) scaledS2 / (1 + totalWeight)
      mu = weightedAvg
      scale = sqrt $ (1 + 1 / totalWeight) * weightedVar
      dof = totalWeight
    

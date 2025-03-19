import LocEst.MathUtils

import Numeric.LinearAlgebra
import           Statistics.Distribution.StudentT  (StudentT,
                                                    studentTUnstandardized)
import           Statistics.Distribution.Transform (LinearTransform)
import           Statistics.Distribution (logDensity, quantile)
import Control.Monad (when)
import Prelude  hiding ((<>))
import Data.List (singleton)

main :: IO ()
main = do
    -- Convert the arguments to the appropriate types
    let x = fromList [ 0.6152178, 6.2557499, 2.0140909, 2.4989887, 3.4760168, 4.9998595
            , 5.6690043, 5.2020506, 3.4362923, 0.2459829, 4.1285186, 5.6877753
            , 3.7348048, 5.6415202, 1.3087586, 4.7733041, 4.0351114, 6.2180439
            , 2.3231876, 2.2225948
            ]
        y = fromList [  0.6560150, -0.1904953,  0.7284932,  0.6543980, -0.4639319, -1.0816896
            , -0.2914018, -0.8762713, -0.3765554,  0.7665486, -0.9453003, -0.4434826
            , -0.9415059, -0.5509706,  0.9326286, -1.1088899, -0.7854040,  0.1428688
            ,  0.6361191,  1.3020555
            ]
        t = 3
        xx = linspace 100 (0, 2*pi)
    
    let res = kernelAverageSmoothing x y t xx
    
    putStrLn $ show res

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
kernelAverageSmoothing :: Vector R -> Vector R -> Double -> Vector R -> [(Double, Double, Double)]
kernelAverageSmoothing x y t xx = do
    zipWith3 queryDistribution (toList mu) (toList scale) (toList dof)
    where
      values = fromRows $ replicate (size xx) y
      dists = distanceMatrix (fromColumns [xx]) (fromColumns [x]) -- + scalar 0.01
      weights = cmap (exp . negate) (dists / scalar t ^ 2)
      totalWeight = rowSums weights
      weightedAvg = rowSums (values * weights) / totalWeight
      weightedVarBasic = rowSums (weights * (values - asColumn weightedAvg) ^ 2) / (totalWeight - 1)
      scaledS2 = (totalWeight - 1) * weightedVarBasic
      varSample = sumElements (cmap (\x-> (x - vectorMean y)^2) y) / (fromIntegral (size y) - 1)
      weightedVar = cmap (+ varSample) scaledS2 / (1 + totalWeight)
      mu = weightedAvg
      scale = sqrt $ (1 + 1 / totalWeight) * weightedVar
      dof = totalWeight
      queryDistribution :: Double -> Double -> Double -> (Double, Double, Double)
      queryDistribution _mu _scale _dof = 
          case generalizedStudentT _mu _scale _dof of
              Right distribution ->
                  let lower  = quantile distribution 0.025
                      median = quantile distribution 0.5
                      upper  = quantile distribution 0.975
                  in (lower, median, upper)
              Left e -> error $ show e

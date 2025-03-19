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
        y = fromList [  0.3519458, -0.1028953,  0.8762995,  0.7377007, -0.3077417, -0.9717434
            , -0.8770646, -1.0314244, -0.3977553,  0.1238465, -1.0063318, -0.5830154
            , -0.1095557, -0.9017576,  1.2087323, -1.0144639, -0.6856355, -0.1837594
            ,  0.7238106,  0.8185757
            ]
        t = 0.5
        xx = linspace 100 (0, 2*pi)
    
    let res = kernelAverageSmoothing x y t xx
    
    print $ map (\(_,m,_) -> m) res

-- calculate square of Euclidean norm
sqEuclideanNorm :: Vector R -> Vector R -> R
sqEuclideanNorm a b = sumElements $ (a - b) ** 2

-- calculate the distance matrix
distanceMatrix :: Matrix R -> Matrix R -> Matrix R
distanceMatrix m1 m2 =
  let n = rows m1 
      m = rows m2
  in reshape m . fromList $ [sqrt (sqEuclideanNorm (m1 ! i) (m2 ! j)) | i <- [0 .. (n-1)], j <- [0 .. (m-1)]]

rowSums :: Matrix R -> Vector R
rowSums m = vector $ map sumElements (toRows m)

vectorMean :: Vector R -> R
vectorMean v = sumElements v / fromIntegral (size v)

-- Function for computing kernel average smoothing
kernelAverageSmoothing :: Vector R -> Vector R -> Double -> Vector R -> [(Double, Double, Double)]
kernelAverageSmoothing x y t xx = do
    zipWith3 queryDistribution (toList mu) (toList scale) (toList dof)
    --error $ show weightedAvg
    where
      values = fromRows $ replicate (size xx) y
      dists = distanceMatrix (fromColumns [xx]) (fromColumns [x]) -- + scalar 0.01
      weights = 1 / exp ((dists / scalar t) ** 2)
      totalWeight = rowSums weights
      weightedAvg = rowSums (values * weights) / totalWeight
      weightedVarBasic = rowSums (weights * (values - asColumn weightedAvg) ** 2) / (totalWeight - 1)
      scaledS2 = (totalWeight - 1) * weightedVarBasic
      varSample = sumElements (cmap (\x-> (x - vectorMean y) ** 2) y) / (fromIntegral (size y) - 1)
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

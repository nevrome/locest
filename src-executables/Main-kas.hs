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
    
        xMat = asColumn x
        xxMat = asColumn xx
        dists = cmap sqrt (pairwiseD2 xxMat xMat)
    
    let res = kernelAverageSmoothing dists y t Nothing
    
    print $ map (\(_,m,_,_) -> m) res

-- Helper to sum matrix rows (since hmatrix doesn't have sumRows)
sumRows :: Matrix R -> Vector R
sumRows m = flatten $ m <> konst 1 (cols m, 1)

kernelAverageSmoothing :: Matrix R -> Vector R -> Double -> Maybe Double -> [(Double, Double, Double, Maybe Double)]
kernelAverageSmoothing dists y lengthscale maybeSearchValue = zipWith3 queryDistribution (toList mu) (toList scale) (toList dof)
    where
      weights = cmap (\d -> exp (-(d**2)/(lengthscale**2))) dists
      totalWeight = sumRows weights
      weightedAvg = flatten (weights <> asColumn y) / totalWeight
      values = fromRows $ replicate (rows dists) y
      weightedVarBasic = sumRows (weights * (values - asColumn weightedAvg) ** 2) / (totalWeight - 1)
      meanY = sumElements y / fromIntegral (size y)
      varSample = dot (y - scalar meanY) (y - scalar meanY) / fromIntegral (size y - 1)
      scaledS2 = (totalWeight - 1) * weightedVarBasic
      weightedVar = (scaledS2 + scalar varSample) / (totalWeight + 1)
      mu = weightedAvg
      scale = cmap sqrt ((1 + 1/totalWeight) * weightedVar)
      dof = totalWeight
      queryDistribution _mu _scale _dof = 
          case generalizedStudentT _mu _scale _dof of
              Right distribution ->
                  let lower  = quantile distribution 0.025
                      median = quantile distribution 0.5
                      upper  = quantile distribution 0.975
                      logL   = fmap (logDensity distribution) maybeSearchValue -- log-likelihood
                  in (lower, median, upper, logL)
              Left e -> error $ show e

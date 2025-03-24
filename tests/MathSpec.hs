module MathSpec (spec) where

import           LocEst.CoreAlgorithms
import           LocEst.MathUtils

import           Data.List                         (foldl')
import qualified Data.Vector                       as V
import qualified Data.Vector.Storable              as VS
import qualified Numeric.LinearAlgebra             as M
import           Statistics.Distribution           (quantile)
import           Statistics.Distribution.StudentT  (StudentT)
import           Statistics.Distribution.Transform (LinearTransform)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck                   hiding (scale)
import           Text.Printf

spec :: Spec
spec = do
    testEqualityFullAndPartialFunctions

testEqualityFullAndPartialFunctions :: Spec
testEqualityFullAndPartialFunctions = describe "KAS algorithm implementation behaves as expected" $ do
    prop "neff" $ forAll valuesAndWeights $ uncurry test_neff
    prop "weightedAvg" $ forAll valuesAndWeights $ uncurry test_weightedAvg
    prop "weightedVarBasic" $ forAll valuesAndWeights $ uncurry test_weightedVarBasic
    prop "weightedVar" $ forAll valuesAndWeights $ uncurry test_weightedVar
    prop "posteriorPredictive" $ forAll valuesAndWeights $ uncurry test_posteriorPredictive
    where
        test_neff :: [Double] -> [Double] -> Bool
        test_neff vals weights =
            let vvals = VS.fromList vals
                mweights = M.fromRows [VS.fromList weights]
                (neff, _, _, _, _) = V.head $ kas mweights vvals
            in neff =~= M.sumElements mweights
        test_weightedAvg :: [Double] -> [Double] -> Bool
        test_weightedAvg vals weights =
            let vvals = VS.fromList vals
                mweights = M.fromRows [VS.fromList weights]
                (_, _, _, mu, _) = V.head $ kas mweights vvals
            in mu =~= weightedAvg vals weights
        test_weightedVarBasic :: [Double] -> [Double] -> Bool
        test_weightedVarBasic vals weights =
            let vvals = VS.fromList vals
                mweights = M.fromRows [VS.fromList weights]
                (_, wvb, _, _, _) = V.head $ kas mweights vvals
            in wvb =~= weightedVarBasic vals weights
        test_weightedVar :: [Double] -> [Double] -> Bool
        test_weightedVar vals weights =
            let vvals = VS.fromList vals
                mweights = M.fromRows [VS.fromList weights]
                (_, _, wv, _, _) = V.head $ kas mweights vvals
            in wv =~= weightedVar vals weights
        test_posteriorPredictive :: [Double] -> [Double] -> Bool
        test_posteriorPredictive vals weights =
            let vvals = VS.fromList vals
                mweights = M.fromRows [VS.fromList weights]
                (_, _, _, _, eitherDistribution) = V.head $ kas mweights vvals
                a = case eitherDistribution of
                    Right distribution -> quantile distribution 0.54
                    Left _             -> nan
                b = case posteriorPredictive vals weights of
                    Right distribution -> quantile distribution 0.54
                    Left _             -> nan
            in  case (a =~= b) of
                    False -> error $ roundToStr 3 a ++ " ~ " ++ roundToStr 3 b
                    True  -> True

-- the results differ slightly
roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr = printf "%0.*f"
(=~=) :: Double -> Double -> Bool
l =~= r = roundToStr 5 l == roundToStr 5 r

-- generators

valuesAndWeights :: Gen ([Double], [Double])
valuesAndWeights = do
  len <- choose (2, 100)
  listA <- vectorOf len (arbitrary :: Gen Double)
  listB <- vectorOf len positiveDouble
  return (listA, listB)

positiveDouble :: Gen Double
positiveDouble = abs `fmap` (arbitrary :: Gen Double) `suchThat` (> 0)

-- reference implementation

-- the following code is not used any more, but remains here as useful documentation of the kas algorithm

-- with Bessel's correction
-- https://en.wikipedia.org/wiki/Variance#Unbiased_sample_variance
varSample :: [Double] -> Double
varSample xs = foldl' (\o x -> o + (x - mean)**2) 0 xs / (n-1)
    where
        mean = avg xs
        n = fromIntegral $ length xs

--sdSample :: [Double] -> Double
--sdSample xs = sqrt $ varSample xs

weightedAvg :: [Double] -> [Double] -> Double
weightedAvg values weights =
    foldl' (\o (v,w) -> o + v * w) 0 (zip values weights) / foldSum weights

--weightedAvg = M.flatten (weights M.<> M.asColumn y) / totalWeight

weightedVarBasic :: [Double] -> [Double] -> Double
weightedVarBasic values weights =
    foldl' (\o (v,w) -> o + w * ((v - weightedMean) ** 2)) 0 (zip values weights) / (neff-1)
    where
        weightedMean = weightedAvg values weights
        neff = foldSum weights

weightedVar :: [Double] -> [Double] -> Double
weightedVar values weights =
    (nu0 * sigma02 + scaledS2) / (nu0 + neff)
    where
        scaledS2 = (neff - 1) * s2
        s2 = weightedVarBasic values weights
        neff = foldSum weights
        nu0 = 1
        sigma02 = varSample values

--weightedSD :: [Double] -> [Double] -> Double
--weightedSD values weights =
--    sqrt $ weightedVar values weights

--weightedSEM :: [Double] -> [Double] -> Double
--weightedSEM values weights =
--    sqrt (weightedVar values weights / neff)
--    where
--        neff = totalWeight
--        totalWeight = foldSum weights

--posteriorMu :: [Double] -> [Double] ->  Either String (LinearTransform StudentT)
--posteriorMu values weights = generalizedStudentT mu scale dof
--    where
--        mu = weightedAvg values weights
--        scale = weightedSD values weights / sqrt neff
--        dof = neff1
--        neff1 = neff - 1
--        neff = totalWeight + 1
--        totalWeight = foldSum weights

posteriorPredictive :: [Double] -> [Double] -> Either String (LinearTransform StudentT)
posteriorPredictive values weights = generalizedStudentT mu scale dof
    where
        mu = weightedAvg values weights
        scale = sqrt ((1 + 1/neff) * weightedVar values weights)
        dof = neff - 1
        neff = foldSum weights + 1

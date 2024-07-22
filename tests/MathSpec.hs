module MathSpec (spec) where

import LocEst.MathUtils

import           Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.Vector.Unboxed as VU
import Test.QuickCheck

spec :: Spec
spec = do
    testEqualityFullAndPartialFunctions

testEqualityFullAndPartialFunctions :: Spec
testEqualityFullAndPartialFunctions = describe "LocEst.MathUtils: full and partial functions behave identically" $ do
    prop "avg == avg_" test_avg
    prop "varSample == varSample_" test_varSample
    prop "weightedAvg == weightedAvg_" $ forAll valuesAndWeights $ \(vals, weights) -> test_weightedAvg vals weights
    where
        test_avg :: [Double] -> Bool
        test_avg [] = True
        test_avg xs =
            let vxs = VU.fromList xs
                vxslength = fromIntegral $ VU.length vxs
            in avg_ vxslength vxs == avg xs
        test_varSample :: [Double] -> Bool
        test_varSample []  = True
        test_varSample [_] = True
        test_varSample xs  =
            let vxs = VU.fromList xs
                vxslength = fromIntegral $ VU.length vxs
            in varSample_ vxslength vxs == varSample xs
        test_weightedAvg :: [Double] -> [Double] -> Bool
        test_weightedAvg vals weights =
            let vvals = VU.fromList vals
                vweights = VU.fromList weights
                totalWeight = VU.sum vweights
            in weightedAvg_ totalWeight vvals vweights == weightedAvg vals weights

-- generators

valuesAndWeights :: Gen ([Double], [Double])
valuesAndWeights = do
  len <- choose (1, 100)
  listA <- vectorOf len (arbitrary :: Gen Double)
  listB <- vectorOf len positiveDouble
  return (listA, listB)

positiveDouble :: Gen Double
positiveDouble = abs `fmap` (arbitrary :: Gen Double) `suchThat` (> 0)

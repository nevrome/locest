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
    prop "weightedVarBasic == weightedVarBasic_" $ forAll valuesAndWeights $ \(vals, weights) -> test_weightedVarBasic vals weights
    prop "weightedVar == weightedVar_" $ forAll valuesAndWeights $ \(vals, weights) -> test_weightedVar vals weights
    prop "posteriorPredictive == posteriorPredictive_" $ forAll valuesAndWeights $ \(vals, weights) -> test_posteriorPredictive vals weights
    where
        test_avg :: [Double] -> Bool
        test_avg [] = True
        test_avg vals =
            let vvals = VU.fromList vals
                vlength = fromIntegral $ VU.length vvals
            in avg_ vlength vvals == avg vals
        test_varSample :: [Double] -> Bool
        test_varSample []  = True
        test_varSample [_] = True
        test_varSample vals  =
            let vvals = VU.fromList vals
                vlength = fromIntegral $ VU.length vvals
            in varSample_ vlength vvals == varSample vals
        test_weightedAvg :: [Double] -> [Double] -> Bool
        test_weightedAvg vals weights =
            let vvals = VU.fromList vals
                vweights = VU.fromList weights
                totalWeight = VU.sum vweights
            in weightedAvg_ totalWeight vvals vweights == weightedAvg vals weights
        test_weightedVarBasic :: [Double] -> [Double] -> Bool
        test_weightedVarBasic vals weights =
            let vvals = VU.fromList vals
                vweights = VU.fromList weights
                totalWeight = VU.sum vweights
                weightedM = weightedAvg_ totalWeight vvals vweights
            in weightedVarBasic_ totalWeight weightedM vvals vweights == weightedVarBasic vals weights
        test_weightedVar :: [Double] -> [Double] -> Bool
        test_weightedVar vals weights =
            let vvals = VU.fromList vals
                vweights = VU.fromList weights
                vlength = fromIntegral $ VU.length vvals
                sampleVariance = varSample_ vlength vvals
                totalWeight = VU.sum vweights
                weightedM = weightedAvg_ totalWeight vvals vweights
                weightedVBase = weightedVarBasic_ totalWeight weightedM vvals vweights
            in weightedVar_ sampleVariance weightedVBase totalWeight == weightedVar vals weights
        test_posteriorPredictive :: [Double] -> [Double] -> Bool
        test_posteriorPredictive vals weights =
            let vvals = VU.fromList vals
                vweights = VU.fromList weights
                vlength = fromIntegral $ VU.length vvals
                sampleVariance = varSample_ vlength vvals
                totalWeight = VU.sum vweights
                weightedM = weightedAvg_ totalWeight vvals vweights
                weightedVBase = weightedVarBasic_ totalWeight weightedM vvals vweights
                weightedV = weightedVar_ sampleVariance weightedVBase totalWeight
            in posteriorPredictive_ totalWeight weightedM weightedV == posteriorPredictive vals weights

-- generators

valuesAndWeights :: Gen ([Double], [Double])
valuesAndWeights = do
  len <- choose (2, 100)
  listA <- vectorOf len (arbitrary :: Gen Double)
  listB <- vectorOf len positiveDouble
  return (listA, listB)

positiveDouble :: Gen Double
positiveDouble = abs `fmap` (arbitrary :: Gen Double) `suchThat` (> 0)

module MathSpec (spec) where

import LocEst.MathUtils

import           Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.Vector.Unboxed as VU

spec :: Spec
spec = do
    testEqualityFullAndPartialFunctions

testEqualityFullAndPartialFunctions :: Spec
testEqualityFullAndPartialFunctions = describe "LocEst.MathUtils: full and partial functions behave identically" $ do
    prop "avg == avg_" test_avg
    prop "varSample == varSample_" test_varSample
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
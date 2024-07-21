module MathSpec (spec) where

import LocEst.MathUtils

import           Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.Vector.Unboxed as VU

spec :: Spec
spec = do
    testEqualityFullAndPartialFunctions

testEqualityFullAndPartialFunctions :: Spec
testEqualityFullAndPartialFunctions = describe "LocEst.MathUtils" $ do
    prop "should yield identical results for full and partial functions" test_avg

    where
        test_avg :: [Double] -> Bool
        test_avg [] = True
        test_avg xs =
            let vxs = VU.fromList xs
                vxslength = fromIntegral $ VU.length vxs
            in avg_ vxslength vxs == avg xs
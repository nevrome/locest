module GoldenSpec (spec) where

import           Control.Monad
import           System.IO
import           System.Process
import           Test.Hspec

spec :: Spec
spec = goldenSpec

goldenSpec :: Spec
goldenSpec =

  describe "locest" $ do

    let goldenTests = [
            "01_basic_spacetime", "02_spacetime_wrong_depvar_in_obs"
            ]

    runTestScripts goldenTests

    where
        runTestScripts :: [String] -> Spec
        runTestScripts tests = do
            forM_ tests $ \testConf -> do
                it ("should be executed correctly: " ++ testConf) $ do
                    let cp = (shell $ "locest search --configFile " ++ testConf ++ ".conf +RTS -N1 -RTS") {
                          cwd = Just "tests/golden/",
                          std_out = CreatePipe,
                          std_err = CreatePipe
                        }
                    (_, Just out, Just err, _) <- createProcess cp
                    hSetBuffering out NoBuffering
                    hSetBuffering err NoBuffering
                    -- compare cli output
                    outExpected <- readFile $ "tests/golden/outCLI/" ++ testConf ++ ".out"
                    outRealRaw  <- liftA2 (\x y -> x ++ filter (/= '\r') y) (hGetContents err) (hGetContents out)
                    let outReal = (unlines . drop 3 . lines) outRealRaw
                    outReal `shouldBe` outExpected

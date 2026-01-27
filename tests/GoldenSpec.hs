module GoldenSpec (spec) where

import           Control.Monad
import           System.IO
import           System.Process
import           Test.Hspec
import Numeric (showFFloat)

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
                    let cp = (shell $ "locest search --configFile " ++ testConf ++ ".conf") {
                          cwd = Just "tests/golden/",
                          std_out = CreatePipe,
                          std_err = CreatePipe
                        }
                    (_, Just out, Just err, _) <- createProcess cp
                    hSetBuffering out NoBuffering
                    hSetBuffering err NoBuffering
                    -- compare cli output
                    outExpectedRaw <- readFile $ "tests/golden/outCLI/" ++ testConf ++ ".out"
                    outRealRaw  <- liftA2 (\x y -> x ++ filter (/= '\r') y) (hGetContents err) (hGetContents out)
                    let outExpected = normalize outExpectedRaw
                        outReal = normalize (unlines . drop 3 . lines $ outRealRaw)
                    outReal `shouldBe` outExpected

-- before comparing, round all floats to a fixed precision.
normalize :: String -> String
normalize = unlines . map normLine . lines
  where
    normLine = unwords . map normTok . words
    normTok tok =
      case reads tok :: [(Double, String)] of
        [(x,"")] -> showFFloat (Just 5) x ""
        _        -> tok
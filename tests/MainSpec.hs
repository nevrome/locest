module MainSpec (spec) where

import           Control.Applicative
import           Control.Monad
import           System.IO
import           System.Process
import           Test.Hspec          (Spec, describe, it, shouldReturn)

spec :: Spec
spec = locestSpec

locestSpec :: Spec
locestSpec =

  describe "locest" $ do

    let goldenTests = [
            "01_basic_spacetime"
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
                    outExpected <- readFile $ "tests/golden/outCLI/" ++ testConf ++ ".out"
                    liftA2 (\x y -> x ++ filter (/= '\r') y) (hGetContents err) (hGetContents out)
                        `shouldReturn` outExpected

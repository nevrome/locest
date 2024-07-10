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
            --"01_read_observations"
            ]

    runTestScripts goldenTests

    where
        runTestScripts :: [String] -> Spec
        runTestScripts tests = do
            forM_ tests $ \test -> do
                it ("should be executed correctly: " ++ test) $ do
                    let cp = (shell ("bash " ++ test ++ ".sh")) {
                          cwd = Just "tests/golden/",
                          std_out = CreatePipe,
                          std_err = CreatePipe
                        }
                    (_, Just out, Just err, _) <- createProcess cp
                    hSetBuffering out NoBuffering
                    hSetBuffering err NoBuffering
                    -- compare cli output
                    outExpected <- readFile $ "tests/golden/outCLI/" ++ test ++ ".out"
                    liftA2 (++) (hGetContents out) (hGetContents err) `shouldReturn` outExpected

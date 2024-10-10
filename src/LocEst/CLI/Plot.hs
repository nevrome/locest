{-# LANGUAGE QuasiQuotes #-}
module LocEst.CLI.Plot where

import           Language.R.Instance as R
import           Language.R.QQ

data PlotOptions = PlotOptions {
    test :: String
}

hello :: String -> R s ()
hello name = do
    _ <- [r| print(s_hs) |]
    return ()
  where
    s = "Hello, " ++ name ++ "!"

runPlot :: PlotOptions -> IO ()
runPlot (PlotOptions name) = do
    R.withEmbeddedR R.defaultConfig $ R.runRegion $ hello name

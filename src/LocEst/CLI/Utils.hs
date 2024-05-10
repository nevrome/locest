module LocEst.CLI.Utils where

import           LocEst.Types

import           GHC.Conc                      (getNumCapabilities)
import           System.IO (hPutStrLn, stderr)

setNumberOfThreads :: NumberOfThreads -> IO Int
setNumberOfThreads SingleThread        = pure 1
setNumberOfThreads (MultipleThreads n) = pure n
setNumberOfThreads DetectThreads       = do
    detectedThreads <- getNumCapabilities
    hPutStrLn stderr $ "Detected max number of threads: " ++ show detectedThreads
    return detectedThreads

module LocEst.CLI.SampleAge where

import           System.IO       (hPutStrLn, stderr)

import qualified Currycarbon as C14

data SampleAgeOptions = SampleAgeOptions {
    _spfsInObservationFile :: FilePath,
    _spfsOutFile           :: FilePath
}

runSampleAge :: SampleAgeOptions -> IO ()
runSampleAge (SampleAgeOptions _ _) = do
    hPutStrLn stderr $ show C14.defaultCalConf
    hPutStrLn stderr "Done"

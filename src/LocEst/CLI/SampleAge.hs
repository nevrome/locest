module LocEst.CLI.SampleAge where

import           System.IO       (hPutStrLn, stderr)

data SampleAgeOptions = SampleAgeOptions {
    _spfsInObservationFile :: FilePath,
    _spfsOutFile           :: FilePath
}

runSampleAge :: SampleAgeOptions -> IO ()
runSampleAge (SampleAgeOptions _ _) = do
    hPutStrLn stderr "Done"

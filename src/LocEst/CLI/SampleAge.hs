{-# LANGUAGE BangPatterns #-}

module LocEst.CLI.SampleAge where

import LocEst.Parsers
import LocEst.Types

import           System.IO       (hPutStrLn, stderr)
import qualified Currycarbon as C14


data SampleAgeOptions = SampleAgeOptions {
    _spfsInObservationFile :: FilePath,
    _spfsOutFile           :: FilePath
}

runSampleAge :: SampleAgeOptions -> IO ()
runSampleAge (SampleAgeOptions inObsFile _) = do
    hPutStrLn stderr $ show C14.defaultCalConf
    !allObservationAges <- readObservationAges inObsFile
    let hu = map _obsAgeRaw allObservationAges
    hPutStrLn stderr $ show hu

    hPutStrLn stderr "Done"

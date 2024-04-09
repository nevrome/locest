module LocEst.CLI.Vario where

import           System.IO       (hPutStrLn, stderr)

data VarioOptions = VarioOptions {
    _voInObservationFile :: FilePath,
    _voVariogramOutFile  :: Maybe FilePath
}

runVario :: VarioOptions -> IO ()
runVario _ = hPutStrLn stderr "todo"

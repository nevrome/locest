module LocEst.CLI.Serialise where

import qualified Codec.Serialise as S
import           LocEst.Parsers
import           LocEst.Types
import           System.IO       (hPutStrLn, stderr)
import qualified Data.Vector as V

data SerialiseOptions = SerialiseOptions {
      _serialiseSet     :: SerialiseSet
    , _serialiseOutFile :: FilePath
}

data SerialiseSet =
      SerialiseObsFile {
        _sofInObservationFile :: FilePath
      }
    | SerialiseSpatDistFile  {
        _spfsInSpatDistFile    :: FilePath,
        _spfsInObservationFile :: FilePath,
        _spfsInInSpatGridFile  :: FilePath,
        _spfsNoOrderCheck      :: Bool
    }

runSerialise :: SerialiseOptions -> IO ()
runSerialise (SerialiseOptions (SerialiseObsFile inObsFile) outFile) = do
    observations <- readObservations inObsFile
    hPutStrLn stderr $ "Serialising observations to " ++ outFile
    S.writeFileSerialise outFile observations
    hPutStrLn stderr "Done"
runSerialise (SerialiseOptions (SerialiseSpatDistFile inSpatDistFile inObsFile inSpatGridFile noOrderCheck) outFile) = do
    -- read input
    observations <- readObservations inObsFile
    inSpatGrid <- readSpatPos inSpatGridFile
    inSpatDists <- readSpatDist noOrderCheck observations inSpatGrid inSpatDistFile
    -- serialise output
    hPutStrLn stderr $ "Serialising spatial distances to " ++ outFile
    S.writeFileSerialise outFile inSpatDists
    hPutStrLn stderr "Done"

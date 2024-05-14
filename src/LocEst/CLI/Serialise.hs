module LocEst.CLI.Serialise where

import qualified Codec.Serialise as S
import           LocEst.Parsers
import           System.IO       (hPutStrLn, stderr)

data SerialiseOptions = SerialiseOptions {
      _serialiseSet     :: SerialiseSet
    , _serialiseOutFile :: FilePath
}

data SerialiseSet =
      SerialiseObsFile {
        _sofInObservationFile  :: FilePath
      }
    | SerialiseSpatGridFile {
        _ssgfInSpatGridFile    :: FilePath
      }
    | SerialiseAnyGridFile {
        _sagfInAnyGridFile     :: FilePath
      }
    | SerialiseSpatDistFile  {
        _spfsInSpatDistFile    :: FilePath,
        _spfsInObservationFile :: FilePath,
        _spfsInSpatGridFile    :: FilePath,
        _spfsNoOrderCheck      :: Bool
    }

runSerialise :: SerialiseOptions -> IO ()
runSerialise (SerialiseOptions (SerialiseObsFile inObsFile) outFile) = do
    observations <- readObservations inObsFile
    hPutStrLn stderr $ "Serialising to " ++ outFile
    S.writeFileSerialise outFile observations
    hPutStrLn stderr "Done"
runSerialise (SerialiseOptions (SerialiseSpatGridFile inSpatGridFile) outFile) = do
    inSpatGrid <- readSpatPos inSpatGridFile
    hPutStrLn stderr $ "Serialising to " ++ outFile
    S.writeFileSerialise outFile inSpatGrid
    hPutStrLn stderr "Done"
runSerialise (SerialiseOptions (SerialiseAnyGridFile inAnyGridFile) outFile) = do
    inAnyGrid <- readArbitraryDimPos inAnyGridFile
    hPutStrLn stderr $ "Serialising to " ++ outFile
    S.writeFileSerialise outFile inAnyGrid
    hPutStrLn stderr "Done"
runSerialise (SerialiseOptions (SerialiseSpatDistFile inSpatDistFile inObsFile inSpatGridFile noOrderCheck) outFile) = do
    -- read input
    observations <- readObservations inObsFile
    inSpatGrid <- readSpatPos inSpatGridFile
    inSpatDists <- readSpatDist (ReadSpatDistParse noOrderCheck observations inSpatGrid inSpatDistFile)
    -- serialise output
    hPutStrLn stderr $ "Serialising to " ++ outFile
    S.writeFileSerialise outFile inSpatDists
    hPutStrLn stderr "Done"

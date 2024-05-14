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
    write outFile observations
runSerialise (SerialiseOptions (SerialiseSpatGridFile inSpatGridFile) outFile) = do
    inSpatGrid   <- readSpatPos inSpatGridFile
    write outFile inSpatGrid
runSerialise (SerialiseOptions (SerialiseAnyGridFile inAnyGridFile) outFile) = do
    inAnyGrid    <- readArbitraryDimPos inAnyGridFile
    write outFile inAnyGrid
runSerialise (SerialiseOptions (SerialiseSpatDistFile inSpatDistFile inObsFile inSpatGridFile noOrderCheck) outFile) = do
    observations <- readObservations inObsFile
    inSpatGrid   <- readSpatPos inSpatGridFile
    inSpatDists  <- readSpatDist (ReadSpatDistParse noOrderCheck observations inSpatGrid inSpatDistFile)
    write outFile inSpatDists

write :: S.Serialise a => FilePath -> a -> IO ()
write path x = do
    hPutStrLn stderr $ "Serialising to " ++ path
    S.writeFileSerialise path x
    hPutStrLn stderr "Done"
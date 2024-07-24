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
        _sofInObservationFile     :: FilePath
      }
    | SerialiseSpatGridFile {
        _ssgfInSpatGridFile       :: FilePath
      }
    | SerialiseAnyGridFile {
        _sagfInAnyGridFile        :: FilePath
      }
    | SerialiseObsObsSpatDistFile {
        _sooInSpatDistFile    :: FilePath
      , _sooInObservationFile :: FilePath
      , _sooNoOrderCheck      :: Bool
      }
    | SerialiseSpatDistFile  {
         _spfsInSpatDistFile   :: FilePath
      , _spfsInObservationFile :: FilePath
      , _spfsInSpatGridFile    :: FilePath
      , _spfsNoOrderCheck      :: Bool
      }
    | SerialiseObsTempSamplesFile  {
        _sotsInObservationFile    :: FilePath
      , _sotsInObsTempSamplesFile :: FilePath
      , _sotsNoOrderCheck         :: Bool
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
runSerialise (SerialiseOptions (SerialiseObsObsSpatDistFile inSpatDistFile inObsFile noOrderCheck) outFile) = do
    observations <- readObservations inObsFile
    inSpatDists  <- readSpatDist (ReadSpatDistParse noOrderCheck observations Nothing inSpatDistFile)
    write outFile inSpatDists
runSerialise (SerialiseOptions (SerialiseSpatDistFile inSpatDistFile inObsFile inSpatGridFile noOrderCheck) outFile) = do
    observations <- readObservations inObsFile
    inSpatGrid   <- readSpatPos inSpatGridFile
    inSpatDists  <- readSpatDist (ReadSpatDistParse noOrderCheck observations (Just inSpatGrid) inSpatDistFile)
    write outFile inSpatDists
runSerialise (SerialiseOptions (SerialiseObsTempSamplesFile inObsFile inObsTempSamplesFile noOrderCheck) outFile) = do
    observations <- readObservations inObsFile
    inTempSamps  <- readTempSamp (ReadTempSampParse noOrderCheck observations inObsTempSamplesFile)
    write outFile inTempSamps

write :: S.Serialise a => FilePath -> a -> IO ()
write path x = do
    hPutStrLn stderr $ "Serialising to " ++ path
    S.writeFileSerialise path x
    hPutStrLn stderr "Done"

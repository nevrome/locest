module LocEst.CLI.Serialise where

import qualified Codec.Serialise as S
import qualified Data.Vector     as V
import           LocEst.Parsers
import           System.IO       (hPutStrLn, stderr)

data SerialiseOptions = SerialiseOptions {
      _serialiseSet     :: SerialiseSet
    , _serialiseOutFile :: FilePath
}

data SerialiseSet =
      SerialiseObsFile      { _sofInObservationFile :: FilePath }
    | SerialiseSpatGridFile { _ssgfInSpatGridFile   :: FilePath }
    | SerialiseAnyGridFile  { _sagfInAnyGridFile    :: FilePath }
    | SerialiseObsTempSamplesFile  {
        _sotsInObservationFile    :: FilePath
      , _sotsInObsTempSamplesFile :: FilePath
      }
    | SerialiseSUDistMatrixPerIndepVar {
        ssudRefVecFile :: VecFile
      , ssudInDistFile :: FilePath
    }
    | SerialiseAUDistMatrixPerIndepVar {
        saudRefObsFile  :: FilePath
      , saudRefGridFile :: FilePath
      , saudInDistFile  :: FilePath
    }

data VecFile = VecFileObs FilePath | VecFileGrid FilePath

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
runSerialise (SerialiseOptions (SerialiseObsTempSamplesFile inObsFile inObsTempSamplesFile) outFile) = do
    observations <- readObservations inObsFile
    inTempSamps  <- readTempSamp observations inObsTempSamplesFile
    write outFile inTempSamps
runSerialise (SerialiseOptions (SerialiseSUDistMatrixPerIndepVar vecFile distFile) outFile) = do
    nVec <- case vecFile of
        VecFileObs path  -> V.length <$> readObservations path
        VecFileGrid path -> V.length <$> readIndepVarsPos path
    res <- readSUDistMulti nVec distFile
    write outFile res
runSerialise (SerialiseOptions (SerialiseAUDistMatrixPerIndepVar obsFile gridFile distFile) outFile) = do
    nObs <- V.length <$> readObservations obsFile
    nGrid <- V.length <$> readIndepVarsPos gridFile
    res <- readAUDistMulti nObs nGrid distFile
    write outFile res

write :: S.Serialise a => FilePath -> a -> IO ()
write path x = do
    hPutStrLn stderr $ "Serialising to " ++ path
    S.writeFileSerialise path x
    hPutStrLn stderr "Done"

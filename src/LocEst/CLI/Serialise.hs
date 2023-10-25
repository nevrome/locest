module LocEst.CLI.Serialise where

import           LocEst.Types
import qualified Codec.Serialise as S
import LocEst.Parsers
import           System.IO                     (hPutStrLn, stderr)

data SerialiseOptions = SerialiseSpatDistFile SpatDistFileSettings

data SpatDistFileSettings = SpatDistFileSettings {
    _spfsInSpatDistFile    :: FilePath,
    _spfsInObservationFile :: FilePath,
    _spfsInInSpatGridFile  :: FilePath,
    _spfsNoOrderCheck      :: Bool,
    _spfsOutFile           :: FilePath
}

runSerialise :: SerialiseOptions -> IO ()
runSerialise (SerialiseSpatDistFile (SpatDistFileSettings inSpatDistFile inObsFile inSpatGridFile noOrderCheck outFile)) = do
    -- read input
    allObservationsUnindexed <- readObservations inObsFile
    let allObservations = zipWith setIndex allObservationsUnindexed [0..]
    inSpatGridUnindexed <- readSpatPos inSpatGridFile
    let inSpatGrid = zipWith setIndex inSpatGridUnindexed [0..]
    inSpatDists <- readSpatDist noOrderCheck allObservations inSpatGrid inSpatDistFile
    -- serialise output
    hPutStrLn stderr $ "Serialising spatial distances to " ++ outFile
    S.writeFileSerialise outFile inSpatDists
    hPutStrLn stderr "Done"

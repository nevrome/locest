module LocEst.CLI.Serialise where

import qualified Codec.Serialise as S
import           LocEst.Parsers
import           LocEst.Types
import           System.IO       (hPutStrLn, stderr)
import qualified Data.Vector as V

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
    observationsUnindexed <- readObservations inObsFile
    let observations = V.zipWith setIndex observationsUnindexed (V.generate (V.length observationsUnindexed) id)
    inSpatGridUnindexed <- readSpatPos inSpatGridFile
    let inSpatGrid = V.zipWith setIndex inSpatGridUnindexed (V.generate (V.length inSpatGridUnindexed) id)
    inSpatDists <- readSpatDist noOrderCheck observations inSpatGrid inSpatDistFile
    -- serialise output
    hPutStrLn stderr $ "Serialising spatial distances to " ++ outFile
    S.writeFileSerialise outFile inSpatDists
    hPutStrLn stderr "Done"

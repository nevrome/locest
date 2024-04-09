{-# LANGUAGE BangPatterns        #-}

module LocEst.CLI.Vario where

import LocEst.Parsers
import LocEst.Types

import           System.IO       (hPutStrLn, stderr)

data VarioOptions = VarioOptions {
    _voInObservationFile :: FilePath,
    _voVariogramOutFile  :: Maybe FilePath
}

runVario :: VarioOptions -> IO ()
runVario (VarioOptions inObsFile _) = do
    -- read observations
    hPutStrLn stderr "Reading observations"
    !observationsUnindexed <- readObservations inObsFile
    let observations = zipWith setIndex observationsUnindexed [0..]
    -- pairwise distances
    hPutStrLn stderr "Calculating pairwise distances"

    -- huhu
    hPutStrLn stderr "wip"

calculatePairwiseDistances :: [Observation] -> SUDistMatrix
calculatePairwiseDistances obs =
    undefined
    
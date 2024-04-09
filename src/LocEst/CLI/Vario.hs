{-# LANGUAGE BangPatterns        #-}

module LocEst.CLI.Vario where

import LocEst.Parsers
import LocEst.Types
import LocEst.Distance

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
    let hu = calculatePairwiseDistances observations
    -- huhu
    hPutStrLn stderr "wip"

calculatePairwiseDistances :: [Observation] -> SUDistMatrix
calculatePairwiseDistances obs =
    undefined

obsobsDist :: Observation -> Observation -> [(String, Double)]
obsobsDist
    (Observation _ _ (HyperPos (IndepSpatTempPos p1) _))
    (Observation _ _ (HyperPos (IndepSpatTempPos p2) _)) =
    [("space", spatialDistSpatTempPos p1 p2), ("time", temporalDistSpatTempPos p1 p2)]
obsobsDist
    (Observation _ _ (HyperPos (IndepArbitraryDimPos p1) _))
    (Observation _ _ (HyperPos (IndepArbitraryDimPos p2) _)) =
    -- this assumes that p1 and p2 have the same order of indep variables
    zip (getKeys p1) (allDistances (getValues p1) (getValues p2))
obsobsDist _ _ = error "Impossible state in obsobsDist"

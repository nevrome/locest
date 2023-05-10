{-# LANGUAGE ScopedTypeVariables   #-}

module LocEst.CLI.Search where

import LocEst.Types
import LocEst.Parsers
import LocEst.Distance
import Data.List (foldl')

data SearchOptions = SearchOptions
    { _searchInObservationFile :: FilePath
    , _searchInSearchPosFile   :: FilePath
    , _searchOutFile           :: FilePath
    }

runSearch :: SearchOptions -> IO ()
runSearch (
    SearchOptions inObsFile inSearchPosFile outFile
    ) = do
    
    allObservations <- readSpatTempObs inObsFile
    pipeSpatTempPosConduit inSearchPosFile outFile (myFunc allObservations)


myFunc :: [SpatTempObs] -> SpatTempPos -> SpatTempProb
myFunc allSpatTempObs spatTempPos =
    let allSpatDists = map (spatialDistSpatTempPos (spatTempPos) . _stpoSpatTempPos) allSpatTempObs
        --allTempDists = map (temporalDistSpatTempPos (spatTempPos) . _stpoSpatTempPos) allSpatTempObs
    in SpatTempProb {
              _stprspatTempPos = spatTempPos
            , _stprprobability = avg allSpatDists
       }

avg :: [Double] -> Double
avg xs = let sum_ = foldl' (+) 0 xs
         in sum_ / fromIntegral (length xs)
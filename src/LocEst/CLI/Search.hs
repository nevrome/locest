{-# LANGUAGE ScopedTypeVariables   #-}

module LocEst.CLI.Search where

import LocEst.Types
import LocEst.Parsers
import LocEst.Distance
import LocEst.Math
import Data.List (zip5)

data SearchOptions = SearchOptions
    { _searchInObservationFile :: FilePath
    , _searchInSpatGridFile    :: FilePath
    , _searchInTempGrid        :: [Int]
    , _searchOutFile           :: FilePath
    }

runSearch :: SearchOptions -> IO ()
runSearch (
    SearchOptions inObsFile inSpatGridFile inTempGrid outFile
    ) = do
    
    print inTempGrid

    allObservations <- readSpatTempObs inObsFile
    pipeSpatTempPosConduit inSpatGridFile outFile (myFunc allObservations)


myFunc :: [SpatTempObs] -> SpatTempPos -> SpatTempProb
myFunc allSpatTempObs spatTempPosRaw =
    let spatTempPos = spatTempPosRaw {_temporalPos = SimpleYearBCAD (-5500) }
        allSpatDists   = map (spatialDistSpatTempPos spatTempPos . _stpoSpatTempPos) allSpatTempObs
        allSpatDistsKM = map (/ 1000) allSpatDists
        allTempDists   = map (temporalDistSpatTempPos spatTempPos . _stpoSpatTempPos) allSpatTempObs
        allPCMeans     = map _stpopc1 allSpatTempObs
        allPCSDs       = map (\(s,t) -> 0.0001 * s + 0.0001 * t) (zip allSpatDistsKM allTempDists)
        allDensities   = map (\(mean,sd) -> dnorm mean sd 0.0461299) (zip allPCMeans allPCSDs)
        minPC1         = minimum allPCMeans
        maxPC1         = maximum allPCMeans
        allIntegrals   = map (\(mean,sd) -> integrate 100 (dnorm mean sd) minPC1 maxPC1) (zip allPCMeans allPCSDs)
        meanDens       = 
            -- avg allDensities -- too smooth, low densities pull the mean down
            maximum allDensities -- too aggressive?
            --weightedAvg allIntegrals allDensities



    in --error $ show $ zip5 allSpatDistsKM allTempDists allPCMeans allPCSDs allDensities
       SpatTempProb { _stprspatTempPos = spatTempPos, _stprprobability = meanDens }

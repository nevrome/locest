{-# LANGUAGE ScopedTypeVariables #-}

module LocEst.CLI.Search where

import           LocEst.Distance
import           LocEst.Math.Basics
import           LocEst.Parsers
import           LocEst.Types

import           Data.Conduit                  ((.|))
import qualified Data.Conduit                  as Con
import qualified Data.Conduit.Algorithms.Async as ConAA
import qualified Data.Conduit.List             as ConL

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
    -- pipeSpatTempPosConduit inSpatGridFile outFile (myFunc allObservations)

    Con.runConduitRes $
           sourceCSV inSpatGridFile
        -- multiply spatial input grid by temporal grid
        .| ConL.concatMap (multiplySpatPosByTempGrid inTempGrid)
        -- .| ConL.map myFunc -- sequential
        .| ConAA.asyncMapC 5 (myFunc allObservations) -- normal parallel
        -- .| Con.conduitVector 100 .| ConAA.asyncMapC 5 (V.map myFunc) .| ConL.concat -- chunked parallel
        .| progress
        .| sinkCSV outFile

myFunc :: [SpatTempObs] -> SpatTempPos -> SpatTempProb
myFunc allSpatTempObs spatTempPos =
    let allSpatDists   = map (spatialDistSpatTempPos spatTempPos . _stpoSpatTempPos) allSpatTempObs
        allSpatDistsKM = map (/ 1000) allSpatDists
        allTempDists   = map (temporalDistSpatTempPos spatTempPos . _stpoSpatTempPos) allSpatTempObs
        allPCMeans     = map _stpopc1 allSpatTempObs
        allPCSDs       = map (\(s,t) -> 0.0001 * s + 0.0001 * t) (zip allSpatDistsKM allTempDists)
        allDensities   = map (\(mean,sd) -> dnorm mean sd 0.0461299) (zip allPCMeans allPCSDs)
        --minPC1         = minimum allPCMeans
        --maxPC1         = maximum allPCMeans
        --allIntegrals   = map (\(mean,sd) -> integrate 100 (dnorm mean sd) minPC1 maxPC1) (zip allPCMeans allPCSDs)
        meanDens       =
            -- avg allDensities -- too smooth, low densities pull the mean down
            maximum allDensities -- too aggressive?
            --weightedAvg allIntegrals allDensities


    in --error $ show $ zip5 allSpatDistsKM allTempDists allPCMeans allPCSDs allDensities
       SpatTempProb { _stprspatTempPos = spatTempPos, _stprprobability = meanDens }

multiplySpatPosByTempGrid :: [Int] -> SpatPos -> [SpatTempPos]
multiplySpatPosByTempGrid tempGrid spatPos =
    map (\y -> SpatTempPos { _spatialPos = spatPos, _temporalPos = SimpleYearBCAD y}) tempGrid


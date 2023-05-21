{-# LANGUAGE ScopedTypeVariables #-}

module LocEst.CLI.Interpolate where

import           LocEst.Distance
--import           LocEst.Math.Basics
import           LocEst.Parsers
import           LocEst.Types

import           Data.Conduit                   ((.|))
import qualified Data.Conduit                   as Con
import qualified Data.Conduit.Algorithms.Async  as ConAA
import qualified Data.Conduit.List              as ConL
import qualified Data.HashMap.Strict            as HM
import           LocEst.Math.MultivariateNormal (dnormMulti)

data InterpolateOptions = InterpolateOptions
    { _interpolateInObservationFile :: FilePath
    , _interpolateInSpatGridFile    :: FilePath
    , _interpolateInTempGrid        :: [Int]
    , _interpolateSearchDepVars     :: DepVarsMap
    , _interpolateOutFile           :: FilePath
    }

runInterpolate :: InterpolateOptions -> IO ()
runInterpolate (
    InterpolateOptions inObsFile inSpatGridFile inTempGrid searchDepVars outFile
    ) = do
    allObservations <- readSpatTempObs inObsFile
    Con.runConduitRes $
           sourceCSV inSpatGridFile
        -- multiply spatial input grid by temporal grid
        .| ConL.concatMap (multiplySpatPosByTempGrid inTempGrid)
        .| ConAA.asyncMapC 5 (myFunc searchDepVars allObservations) -- normal parallel
        .| progress
        .| sinkCSV outFile

myFunc :: DepVarsMap -> [SpatTempObs] -> SpatTempPos -> SpatTempProb
myFunc searchDepVarMap allSpatTempObs spatTempPos =
    let depVarOrder    = HM.keys $ getHM searchDepVarMap
        searchDepVars  = depVarsExtractOrdered depVarOrder searchDepVarMap
        allSpatDists   = map (spatialDistSpatTempPos spatTempPos . _stpoSpatTempPos) allSpatTempObs
        allSpatDistsKM = map (/ 1000) allSpatDists
        allTempDists   = map (temporalDistSpatTempPos spatTempPos . _stpoSpatTempPos) allSpatTempObs
        allPCMeans     = map (depVarsExtractOrdered depVarOrder . _stpoDepVars) allSpatTempObs
        allPCSDs       = map (replicate (length searchDepVars)) $ map (\(s,t) -> 0.0001 * s + 0.0001 * t) (zip allSpatDistsKM allTempDists)
        allDensities   = map (\(mean,sd) -> dnormMulti mean sd searchDepVars) (zip allPCMeans allPCSDs)
        meanDens       = maximum allDensities
    in --error $ show $ zip5 allSpatDistsKM allTempDists allPCMeans allPCSDs allDensities
       SpatTempProb { _stprspatTempPos = spatTempPos, _stprprobability = meanDens }

multiplySpatPosByTempGrid :: [Int] -> SpatPos -> [SpatTempPos]
multiplySpatPosByTempGrid tempGrid spatPos =
    map (\y -> SpatTempPos { _spatialPos = spatPos, _temporalPos = SimpleYearBCAD y}) tempGrid


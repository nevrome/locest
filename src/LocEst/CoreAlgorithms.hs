module LocEst.CoreAlgorithms where

import           LocEst.Distance
import           LocEst.Types
import           LocEst.Math.MultivariateNormal (dnormMulti)

import qualified Data.HashMap.Strict            as HM

coreSearch = propAtSpatTempDepVarsPos

propAtSpatTempDepVarsPos :: [String] -> [SpatTempDepVarsPos] -> SpatTempDepVarsPos -> SpatTempProb
propAtSpatTempDepVarsPos depVarsOrdered inSpatTempDepVarsPos (SpatTempDepVarsPos gridSpatTempPos searchDepVarPos) =
    let searchDepVars  = depVarsExtractOrdered depVarsOrdered searchDepVarPos
        spatDists   = map (spatialDistSpatTempPos gridSpatTempPos . _stpoSpatTempPos) inSpatTempDepVarsPos
        spatDistsKM = map (/ 1000) spatDists
        tempDists   = map (temporalDistSpatTempPos gridSpatTempPos . _stpoSpatTempPos) inSpatTempDepVarsPos
        depVarMeans = map (depVarsExtractOrdered depVarsOrdered . _stpoDepVarsPos) inSpatTempDepVarsPos
        depVarSDs   = map (replicate (length searchDepVars)) $ map (\(s,t) -> 0.0001 * s + 0.0001 * t) (zip spatDistsKM tempDists)
        densities   = map (\(mean,sd) -> dnormMulti mean sd searchDepVars) (zip depVarMeans depVarSDs)
        --minPC1         = minimum allPCMeans
        --maxPC1         = maximum allPCMeans
        --allIntegrals   = map (\(mean,sd) -> integrate 100 (dnorm mean sd) minPC1 maxPC1) (zip allPCMeans allPCSDs)
        meanDens       =
            -- avg allDensities -- too smooth, low densities pull the mean down
            maximum densities -- too aggressive?
            --weightedAvg allIntegrals allDensities
    in --error $ show $ zip5 allSpatDistsKM allTempDists allPCMeans allPCSDs allDensities
        SpatTempProb { _stprspatTempPos = gridSpatTempPos, _stprDepVarsPos = searchDepVarPos, _stprprobability = meanDens }
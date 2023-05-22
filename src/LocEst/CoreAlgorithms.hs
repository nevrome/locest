module LocEst.CoreAlgorithms where

import           LocEst.Distance
import           LocEst.Types
import           LocEst.Math.MultivariateNormal (dnormMulti)

import qualified Data.HashMap.Strict            as HM

coreSearch = propAtMultiDepVarsPos

propAtMultiDepVarsPos :: [SpatTempObs] -> [DepVarsPos] -> SpatTempPos -> [SpatTempProb]
propAtMultiDepVarsPos allSpatTempObs searchDepVarPos spatTempPos = map (\sDVP -> propAtDepVarsPos allSpatTempObs sDVP spatTempPos) searchDepVarPos

propAtDepVarsPos :: [SpatTempObs] -> DepVarsPos -> SpatTempPos -> SpatTempProb
propAtDepVarsPos allSpatTempObs searchDepVarPos spatTempPos =
    let depVarOrder    = HM.keys $ getHM searchDepVarPos
        searchDepVars  = depVarsExtractOrdered depVarOrder searchDepVarPos
        allSpatDists   = map (spatialDistSpatTempPos spatTempPos . _stpoSpatTempPos) allSpatTempObs
        allSpatDistsKM = map (/ 1000) allSpatDists
        allTempDists   = map (temporalDistSpatTempPos spatTempPos . _stpoSpatTempPos) allSpatTempObs
        allPCMeans     = map (depVarsExtractOrdered depVarOrder . _stpoDepVars) allSpatTempObs
        allPCSDs       = map (replicate (length searchDepVars)) $ map (\(s,t) -> 0.0001 * s + 0.0001 * t) (zip allSpatDistsKM allTempDists)
        allDensities   = map (\(mean,sd) -> dnormMulti mean sd searchDepVars) (zip allPCMeans allPCSDs)
        --minPC1         = minimum allPCMeans
        --maxPC1         = maximum allPCMeans
        --allIntegrals   = map (\(mean,sd) -> integrate 100 (dnorm mean sd) minPC1 maxPC1) (zip allPCMeans allPCSDs)
        meanDens       =
            -- avg allDensities -- too smooth, low densities pull the mean down
            maximum allDensities -- too aggressive?
            --weightedAvg allIntegrals allDensities
    in --error $ show $ zip5 allSpatDistsKM allTempDists allPCMeans allPCSDs allDensities
        SpatTempProb { _stprspatTempPos = spatTempPos, _stprDepVarsPos = searchDepVarPos, _stprprobability = meanDens }
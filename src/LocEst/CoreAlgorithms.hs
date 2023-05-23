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
        depVarSDs   = map (replicate (length searchDepVars)) $ zipWith (calculateSD (LinearSum 0.0001 0.0001)) spatDistsKM tempDists
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

data SignalDecayAlgorithm =
      LinearSum Double Double
    | LogSum Double Double
    -- | ...

calculateSD :: SignalDecayAlgorithm -> Double -> Double -> Double
calculateSD (LinearSum spatDecay tempDecay) spatDist tempDist =
    growth2LinearSum spatDecay tempDecay spatDist tempDist
calculateSD (LogSum spatDecay tempDecay) spatDist tempDist =
    growth2LogSum spatDecay tempDecay spatDist tempDist

growth2LinearSum :: Double -> Double -> Double -> Double -> Double
growth2LinearSum spatDecay tempDecay spatDist tempDist = 
    growthLinear spatDecay spatDist + growthLinear tempDecay tempDist

growthLinear :: Double -> Double -> Double
growthLinear factor x = factor * x

growth2LogSum :: Double -> Double -> Double -> Double -> Double
growth2LogSum spatDecay tempDecay spatDist tempDist = 
    growthLog spatDecay spatDist + growthLog tempDecay tempDist

growthLog :: Double -> Double -> Double
growthLog factor x = factor * log x
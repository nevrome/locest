module LocEst.CoreAlgorithms where

import           LocEst.Distance
import           LocEst.Types
import           LocEst.Math.MultivariateNormal (dnormMulti)
import           LocEst.Math.Basics

import qualified Data.HashMap.Strict            as HM

coreSearch = propAtSpatTempDepVarsPos

propAtSpatTempDepVarsPos :: [String] -> [SpatTempDepVarsPos] -> SpatTempDepVarsPos -> SpatTempProb
propAtSpatTempDepVarsPos depVarsOrdered inSpatTempDepVarsPos (SpatTempDepVarsPos gridSpatTempPos searchDepVarPos) =
    let searchDepVarsCoords = depVarsExtractOrdered depVarsOrdered searchDepVarPos
        spatDists   = map (spatialDistSpatTempPos gridSpatTempPos . _stpoSpatTempPos) inSpatTempDepVarsPos
        spatDistsKM = map (/ 1000) spatDists
        tempDists   = map (temporalDistSpatTempPos gridSpatTempPos . _stpoSpatTempPos) inSpatTempDepVarsPos
        depVarMeans = map (depVarsExtractOrdered depVarsOrdered . _stpoDepVarsPos) inSpatTempDepVarsPos
        depVarSDs   = zipWith (\sdist tdist -> map (\depVar -> calcSD myDecay depVar sdist tdist) depVarsOrdered) spatDistsKM tempDists
        densities   = zipWith (\mean sd -> dnormMulti mean sd searchDepVarsCoords) depVarMeans depVarSDs
        meanDens    = case mySummary of
            Maximum -> maximum densities
            Mean    -> avg densities
    in SpatTempProb {
          _stprspatTempPos = gridSpatTempPos
        , _stprDepVarsPos = searchDepVarPos
        , _stprprobability = meanDens
        }

mySummary = Maximum

data DensitySummaryAlgorithm =
      Maximum
    | Mean
    -- | ...

myDecay = DecayDefinition [
      DecayOneDepVar "varC1" (LinearSum 0.0001 0.0001)
    , DecayOneDepVar "varC2" (LinearSum 0.0001 0.0001)
    ]

newtype DecayDefinition = DecayDefinition [DecayOneDepVar]

data DecayOneDepVar = DecayOneDepVar {
      _stddvDepVarName    :: DepVarName
    , _stddvSpatTempDecay :: DecayAlgorithm
    }

type DepVarName = String

data DecayAlgorithm =
      LinearSum Double Double
    | LogSum Double Double
    -- | ...

calcSD :: DecayDefinition -> DepVarName -> Double -> Double -> Double
calcSD (DecayDefinition depVarList) depVarName spatDist tempDist =
    let relevantDecay = filter (\(DecayOneDepVar var _) -> var == depVarName) depVarList
    in run relevantDecay
    where
        run [DecayOneDepVar _ x] = calc x spatDist tempDist
        run _ = error "this should never happen"
        calc :: DecayAlgorithm -> Double -> Double -> Double
        calc (LinearSum spatDecay tempDecay) = growth2LinearSum spatDecay tempDecay
        calc (LogSum spatDecay tempDecay)    = growth2LogSum spatDecay tempDecay

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
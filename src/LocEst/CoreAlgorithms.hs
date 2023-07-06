module LocEst.CoreAlgorithms where

import           LocEst.Distance
import           LocEst.Types
import           LocEst.Math.MultivariateNormal (dnormMulti)
import           LocEst.Math.Basics

import qualified Data.HashMap.Strict            as HM

coreSearch = propAtSpatTempDepVarsPos

propAtSpatTempDepVarsPos ::
       [String]
    -> [SpatTempDepVarsPos]
    -> Maybe SpatDistMap
    -> SpatTempDepVarsPosWithAlgorithms
    -> SpatTempProb
propAtSpatTempDepVarsPos
    depVarsOrdered
    inSpatTempDepVarsPos
    spatDistMap
    (SpatTempDepVarsPosWithAlgorithms
        (SpatTempDepVarsPos gridSpatTempPos searchDepVarPos)
        decayDefinition
        densitySummaryAlgorithm
    ) =

    let searchDepVarsCoords = depVarsExtractOrdered depVarsOrdered searchDepVarPos
        
        spatDists   = case spatDistMap of
            Nothing          -> map (\x -> spatialDistSpatTempPos gridSpatTempPos . _stpoSpatTempPos $ x) inSpatTempDepVarsPos
            Just spatDistMap -> --placeholder
                                map (\x -> spatialDistSpatTempPos gridSpatTempPos . _stpoSpatTempPos $ x) inSpatTempDepVarsPos
                                -- HM.lookup () -- IDs missing yet

        spatDistsKM = map (/ 1000) spatDists
        tempDists   = map (temporalDistSpatTempPos gridSpatTempPos . _stpoSpatTempPos) inSpatTempDepVarsPos

        -- makes it a lot faster, but has bad side effects
        filteredByDists = filter (\(ds,dt,x) -> ds <= 2000 && dt <= 2000) $ zip3 spatDistsKM tempDists inSpatTempDepVarsPos
        filteredInSpatTempDepVarsPos = map (\(_,_,x) -> x) filteredByDists
        filteredSpatDists = map (\(x,_,_) -> x) filteredByDists
        filteredTempDists = map (\(_,x,_) -> x) filteredByDists


        depVarMeans = map (depVarsExtractOrdered depVarsOrdered . _stpoDepVarsPos) filteredInSpatTempDepVarsPos
        depVarSDs   = zipWith (\sdist tdist -> map (\depVar -> 0.005 + calcSD decayDefinition depVar sdist tdist) depVarsOrdered) filteredSpatDists filteredTempDists
        densities   = zipWith (\mean sd -> dnormMulti mean sd searchDepVarsCoords) depVarMeans depVarSDs
        
        meanDens    = case densitySummaryAlgorithm of
            Maximum -> maximum densities
            Mean    -> avg densities
            DistanceWeightedMean -> weightedAvg (zipWith calcWeight filteredSpatDists filteredTempDists) densities

    in SpatTempProb {
          _stprSpatTempDepVarsPosWithAlgos = SpatTempDepVarsPosWithAlgorithms {
                _powialgPosition = SpatTempDepVarsPos {
                  _stpoSpatTempPos = gridSpatTempPos
                , _stpoDepVarsPos  = searchDepVarPos
                },
                _powialgDecayDef = decayDefinition,
                _powialgDensSumAlgo = densitySummaryAlgorithm
            }
        , _stprprobability = meanDens
        }

calcWeight :: Double -> Double -> Double
calcWeight ds dt =
    -- we can not divide by 0, so distances below 1 are set to 1
    -- not very clever, needs a more general solution
    let dsSafe = max ds 1
        dtSafe = max dt 1
    in 1 / sqrt ((dsSafe ** 2) + (dtSafe ** 2))

-- algorithm options - must be transformed to a proper input when it has stabilized
mySummaries = [mySummary]
myDecays = [myDecay]
mySummary = DistanceWeightedMean
myDecay = DecayDefinition [
      DecayOneDepVar "varC1" (LinearSum 0.00001 0.00001)
    , DecayOneDepVar "varC2" (LinearSum 0.00001 0.00001)
    ]

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
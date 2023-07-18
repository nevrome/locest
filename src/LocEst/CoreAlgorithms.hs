module LocEst.CoreAlgorithms where

import           LocEst.Distance
import           LocEst.Math.Basics
import           LocEst.Math.MultivariateNormal (dnormMulti)
import           LocEst.Types
import           LocEst.Utils

import qualified Data.HashMap.Strict            as HM
import           Data.Maybe                     (catMaybes, isNothing)

coreSearch = propAtSpatTempDepVarsPos

propAtSpatTempDepVarsPos ::
       [String]
    -> [Observation]
    -> Maybe SpatDistMap
    -> SpatTempDepVarsPosWithAlgorithms
    -> Either LOCESTException SpatTempProb
propAtSpatTempDepVarsPos
    depVarsOrdered
    observations
    maybeSpatDistMap
    searchSetting@(SpatTempDepVarsPosWithAlgorithms
        (SpatTempDepVarsPos gridSpatTempPos searchDepVarPos)
        (AlgoSepIDW decayDefinition densitySummaryAlgorithm)
    ) =
    let searchDepVarsCoords = depVarsExtractOrdered depVarsOrdered searchDepVarPos
        -- prepare distances
        spatDists   = findSpatDistsObsGrid observations maybeSpatDistMap gridSpatTempPos
        spatDistsKM = map (/ 1000) $ catMaybes spatDists
        tempDists   = findTempDistsObsGrid observations gridSpatTempPos
        -- filter obs by distance: makes it a lot faster, but has bad side effects
        filteredByDists = filterByDists 2000 2000 $ zip3 spatDistsKM tempDists observations
        filteredObs = map (\(_,_,x) -> x) filteredByDists
        filteredSpatDists = map (\(x,_,_) -> x) filteredByDists
        filteredTempDists = map (\(_,x,_) -> x) filteredByDists
        -- determine mean, sd, and resulting probability densities
        depVarMeans = map (depVarsExtractOrdered depVarsOrdered . _stpoDepVarsPos . _obsPos) filteredObs
        depVarSDs   = zipWith (\sdist tdist -> map (\depVar -> 0.005 + calcSD decayDefinition depVar sdist tdist) depVarsOrdered) filteredSpatDists filteredTempDists
        densities   = zipWith (\mean sd -> dnormMulti mean sd searchDepVarsCoords) depVarMeans depVarSDs
        -- summarise densities
        meanDens    = case densitySummaryAlgorithm of
            Maximum -> maximum densities
            Mean    -> avg densities
            DistanceWeightedMean -> weightedAvg (zipWith calcWeight filteredSpatDists filteredTempDists) densities
    in 
       if (any isNothing spatDists)
       then Left $ NormalException "Could not determine distance ..."
       else Right $ SpatTempProb {
          _stprSpatTempDepVarsPosWithAlgos = searchSetting
        , _stprprobability = meanDens
        }

filterByDists :: Double -> Double -> [(Double, Double, Observation)] -> [(Double, Double, Observation)]
filterByDists fs ft = filter (\(ds,dt,_) -> ds <= fs && dt <= ft)

findTempDistsObsGrid :: [Observation] -> SpatTempPos -> [Double]
findTempDistsObsGrid observations gridSpatTempPos = 
    map (temporalDistSpatTempPos gridSpatTempPos . _stpoSpatTempPos . _obsPos) observations

findSpatDistsObsGrid :: [Observation] -> Maybe SpatDistMap -> SpatTempPos -> [Maybe Double]
findSpatDistsObsGrid observations Nothing gridSpatTempPos =
    -- calculate distances
    map (\x -> Just $ spatialDistSpatTempPos gridSpatTempPos . _stpoSpatTempPos . _obsPos $ x) observations
findSpatDistsObsGrid observations (Just (SpatDistMatrixMap spatDistMap)) gridSpatTempPos =
    -- look up distances
    let obsIDs = map getID observations
        gridSpatPosID = getID $ _spatialPos gridSpatTempPos
    in map (\obsID -> HM.lookup (obsID, gridSpatPosID) spatDistMap) obsIDs

calcWeight :: Double -> Double -> Double
calcWeight ds dt =
    -- we can not divide by 0, so distances below 1 are set to 1
    -- not very clever, needs a more general solution
    let dsSafe = max ds 1
        dtSafe = max dt 1
    in 1 / sqrt ((dsSafe ** 2) + (dtSafe ** 2))

-- algorithm options - must be transformed to a proper input when it has stabilized
myAlgos = [myAlgo]
myAlgo = AlgoSepIDW myDecay mySummary
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
        run _                    = error "this should never happen"
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

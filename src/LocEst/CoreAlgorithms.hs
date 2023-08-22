module LocEst.CoreAlgorithms where

import           LocEst.Distance
import           LocEst.Math.Basics
import           LocEst.Math.MultivariateNormal (dnormMulti)
import           LocEst.Types
import           LocEst.Utils

import           Control.Monad.List             (mapAndUnzipM)
import qualified Data.HashMap.Strict            as HM
import           Data.String                    (fromString)

coreSearch ::
       [String]
    -> [Observation]
    -> Maybe SpatDistMap
    -> SpatTempDepVarsPosWithAlgorithms
    -> Either LOCESTException SpatTempProb
coreSearch
    depVarsOrdered
    observations
    maybeSpatDistMap
    searchSetting@(SpatTempDepVarsPosWithAlgorithms
        (SpatTempDepVarsPos gridSpatTempPos searchDepVarPos)
        (AlgoSepIDW decayDefinition densitySummaryAlgorithm)
    ) = do
    -- determine general per-obs statistics
    let searchDepVarsCoords = depVarsExtractOrdered depVarsOrdered searchDepVarPos
    spatDists <- findSpatDistsObsGrid observations maybeSpatDistMap gridSpatTempPos
    let spatDistsKM = map (/ 1000) spatDists
        tempDists   = findTempDistsObsGrid observations gridSpatTempPos
        obsWithDist = zipWith3 addDistsToObs observations spatDistsKM tempDists
    -- filter by dist (for performance)
    --filteredObsWithDists <- filterByDists 2000 2000 obsWithDist
    -- algorithm (to be refactored)
    let filteredObs = map (\(ObsWithDist x _) -> x) obsWithDist
        filteredSpatDists = map (\(ObsWithDist _ (SpatTempDist x _)) -> x) obsWithDist
        filteredTempDists = map (\(ObsWithDist _ (SpatTempDist _ x)) -> x) obsWithDist
        -- determine mean, sd, and resulting probability densities
        depVarMeans = map (depVarsExtractOrdered depVarsOrdered . _stpoDepVarsPos . _obsPos) filteredObs
        depVarSDs   = zipWith (\sdist tdist -> map (\depVar -> 0.005 + calcSD decayDefinition depVar sdist tdist) depVarsOrdered) filteredSpatDists filteredTempDists
        densities   = zipWith (\mean std -> dnormMulti mean std searchDepVarsCoords) depVarMeans depVarSDs
        -- summarise densities
        meanDens    = case densitySummaryAlgorithm of
            Maximum -> maximum densities
            Mean    -> avg densities
            DistanceWeightedMean -> weightedAvg (zipWith calcWeight filteredSpatDists filteredTempDists) densities
    return $ SpatTempProb {
           _stprSpatTempDepVarsPosWithAlgos = searchSetting
         , _stprprobability = meanDens
         }
    where
        calcSD :: DecayDefinition -> DepVarName -> Double -> Double -> Double
        calcSD (DecayDefinition depVarList) depVarName spatDist tempDist =
            let relevantDecay = filter (\(DecayOneDepVar var _) -> var == depVarName) depVarList
            in run relevantDecay
            where
                run [DecayOneDepVar _ x] = calc x
                run _                    = error "this should never happen"
                calc :: DecayAlgorithm -> Double
                calc (LinearSum spatDecay tempDecay) = growth2LinearSum spatDecay tempDecay
                calc (LogSum spatDecay tempDecay)    = growth2LogSum spatDecay tempDecay
                growth2LinearSum :: Double -> Double -> Double
                growth2LinearSum spatDecay tempDecay =
                    growthLinear spatDecay spatDist + growthLinear tempDecay tempDist
                growthLinear :: Double -> Double -> Double
                growthLinear factor x = factor * x
                growth2LogSum :: Double -> Double -> Double
                growth2LogSum spatDecay tempDecay =
                    growthLog spatDecay spatDist + growthLog tempDecay tempDist
                growthLog :: Double -> Double -> Double
                growthLog factor x = factor * log x
        calcWeight :: Double -> Double -> Double
        calcWeight ds dt =
            -- we can not divide by 0, so distances below 1 are set to 1
            -- not very clever, needs a more general solution
            let dsSafe = max ds 1
                dtSafe = max dt 1
            in 1 / sqrt ((dsSafe ** 2) + (dtSafe ** 2))
coreSearch
    depVarsOrdered
    observations
    maybeSpatDistMap
    searchSetting@(SpatTempDepVarsPosWithAlgorithms
        (SpatTempDepVarsPos gridSpatTempPos searchDepVarPos)
        (AlgoKernSmooth kernelDefinition)
    ) = do
    -- determine general per-obs statistics
    let searchDepVarsCoords = depVarsExtractOrdered depVarsOrdered searchDepVarPos
    spatDists <- findSpatDistsObsGrid observations maybeSpatDistMap gridSpatTempPos
    let spatDistsKM = map (/ 1000) spatDists
        tempDists   = findTempDistsObsGrid observations gridSpatTempPos
        obsWithDist = zipWith3 addDistsToObs observations spatDistsKM tempDists
    -- filter by dist (for performance)
    --filteredObsWithDists <- filterByDists 2000 2000 obsWithDist
    -- summarize obs information for each depVar
    (means, errs) <- mapAndUnzipM (determineWeigthedMeansAndSDsForOneDepVar obsWithDist) depVarsOrdered
    -- summarize per-depVar info into a single value
    let density = dnormMulti means errs searchDepVarsCoords
    return $ SpatTempProb {
           _stprSpatTempDepVarsPosWithAlgos = searchSetting
         , _stprprobability = density
         }
    where
        determineWeigthedMeansAndSDsForOneDepVar :: [ObsWithDist] -> DepVarName -> Either LOCESTException (Double, Double)
        determineWeigthedMeansAndSDsForOneDepVar obsWithDist depVar = do
            obsWeights <- mapM (weightForOneObs kernelDefinition) obsWithDist
            let normalizedObsWeights = map (/ sum obsWeights) obsWeights
            obsMeas <- mapM getOneDepVarPos obsWithDist
            let mean = weightedAvg obsMeas normalizedObsWeights
                err  = weightedSD  obsMeas normalizedObsWeights
            return (mean, err)
            where
                weightForOneObs :: KernelDefinition -> ObsWithDist -> Either LOCESTException Double
                weightForOneObs
                    (KernelDefinition kernelsPerDepVar)
                    (ObsWithDist _ (SpatTempDist spatDist tempDist))
                     = do
                        case filter (\(KernelOneDepVar n _) -> n == depVar) kernelsPerDepVar of
                            []                    -> Left  $ NormalException "not in"
                            [KernelOneDepVar _ k] -> Right $ weightByKernel k
                            _                     -> Left  $ NormalException "in more than once"
                    where
                        weightByKernel :: Kernel -> Double
                        weightByKernel (Uniform spatRadius tempRadius) =
                                let spatWeight = if spatDist <= spatRadius then 1 else 0
                                    tempWeight = if tempDist <= tempRadius then 1 else 0
                                in spatWeight * tempWeight
                        weightByKernel (Normal spatSigma tempSigma) =
                                let spatWeight = dnorm 0 spatSigma spatDist
                                    tempWeight = dnorm 0 tempSigma tempDist
                                in spatWeight * tempWeight
                getOneDepVarPos :: ObsWithDist -> Either LOCESTException Double
                getOneDepVarPos (ObsWithDist (Observation _ (SpatTempDepVarsPos _ (DepVarsPos m))) _) =
                    case HM.lookup depVar m of
                        Nothing -> Left $ NormalException "Unknown variable"
                        Just x  -> Right x

filterByDists :: Double -> Double -> [ObsWithDist] -> Either LOCESTException [ObsWithDist]
filterByDists fs ft xs = do
    let res = filter (\(ObsWithDist _ (SpatTempDist ds dt)) -> ds <= fs && dt <= ft) xs
    if length res < 3
    then Left $ NormalException "Less than 3 individuals in subset."
    else Right res

findTempDistsObsGrid :: [Observation] -> SpatTempPos -> [Double]
findTempDistsObsGrid observations gridSpatTempPos =
    map (temporalDistSpatTempPos gridSpatTempPos . _stpoSpatTempPos . _obsPos) observations

findSpatDistsObsGrid :: [Observation] -> Maybe SpatDistMap -> SpatTempPos -> Either LOCESTException [Double]
-- calculate distances
findSpatDistsObsGrid observations Nothing gridSpatTempPos =
    Right $ map (\x -> spatialDistSpatTempPos gridSpatTempPos . _stpoSpatTempPos . _obsPos $ x) observations
-- look up distances
findSpatDistsObsGrid observations (Just (SpatDistMatrixMap spatDistMap)) gridSpatTempPos =
    let obsIDs = map getID observations
        gridSpatPosID = getID $ _spatialPos gridSpatTempPos
        dists = map (\obsID -> HM.lookup (fromString obsID, fromString gridSpatPosID) spatDistMap) obsIDs
    in case sequence dists of
        Nothing -> Left $ NormalException "Distance not in lookup table."
        Just xs -> Right xs

-- algorithm options - must be transformed to a proper input when it has stabilized
myAlgos :: [LocestAlgorithm]
myAlgos = [myAlgo]
myAlgo :: LocestAlgorithm
myAlgo = AlgoSepIDW myDecay mySummary
mySummary :: DensitySummaryAlgorithm
mySummary = DistanceWeightedMean
myDecay :: DecayDefinition
myDecay = DecayDefinition [
      DecayOneDepVar "varC1" (LinearSum 0.00001 0.00001)
    , DecayOneDepVar "varC2" (LinearSum 0.00001 0.00001)
    ]

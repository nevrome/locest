module LocEst.CoreAlgorithms where

import           LocEst.Distance
import           LocEst.Math.Basics
import           LocEst.Math.MultivariateNormal (dnormMulti)
import           LocEst.Types
import           LocEst.Utils

import qualified Data.HashMap.Strict            as HM
import           Data.String                    (fromString)
import Data.List (unzip4)

coreSearch ::
       [String]
    -> [Observation]
    -> Maybe SpatDistMap
    -> Maybe (Double,Double)
    -> SpatTempDepVarsPosWithAlgorithms
    -> Either LOCESTException SearchResult
coreSearch
    depVarsOrdered
    observations
    maybeSpatDistMap
    spaceTimeFilter
    searchSetting@(SpatTempDepVarsPosWithAlgorithms
        (SpatTempDepVarsPos gridSpatTempPos searchDepVarPos)
        (AlgoInverseKernSmooth kernelDefinition densitySummaryAlgorithm)
    ) = do
    -- determine general per-obs statistics
    let searchDepVarsCoords = depVarsExtractOrdered depVarsOrdered searchDepVarPos
    spatDists <- findSpatDistsObsGrid observations maybeSpatDistMap gridSpatTempPos
    let spatDistsKM = map (/ 1000) spatDists
        tempDists   = findTempDistsObsGrid observations gridSpatTempPos
        obsWithDist = zipWith3 addDistsToObs observations spatDistsKM tempDists
    -- filter by dist (for performance)
    filteredObsWithDists <- case spaceTimeFilter of
        Just (spaceFilter,timeFilter) -> do
            let res = filterByDists spaceFilter timeFilter obsWithDist
            if length res < 3
            then Left $ NormalException "Less than 3 individuals in subset."
            else Right res
        Nothing -> pure obsWithDist
    -- summarize obs information for each depVar
    perObs <- mapM (meansAndWeightsOneObs searchDepVarsCoords kernelDefinition depVarsOrdered) filteredObsWithDists
    -- summarise densities
    let meanDens = case densitySummaryAlgorithm of
            Maximum -> maximum perObs
            Mean    -> undefined
            DistanceWeightedMean -> undefined
    return $ SearchResult {
           _srSpatTempDepVarsPosWithAlgos = searchSetting
         , _srInterpolation = Nothing
         , _srProbability = meanDens
         }
coreSearch
    depVarsOrdered
    observations
    maybeSpatDistMap
    spaceTimeFilter
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
    filteredObsWithDists <- case spaceTimeFilter of
        Just (spaceFilter,timeFilter) -> do
            let res = filterByDists spaceFilter timeFilter obsWithDist
            if length res < 3
            then Left $ NormalException "Less than 3 individuals in subset."
            else Right res
        Nothing -> pure obsWithDist
    -- summarize obs information for each depVar
    perDepVar <- mapM (smoothedValueOneDepVar kernelDefinition filteredObsWithDists) depVarsOrdered
    let (means, errs, _, _) = unzip4 perDepVar
    return $ SearchResult {
           _srSpatTempDepVarsPosWithAlgos = searchSetting
         , _srInterpolation = Just $ DepVarsUncertainPos $ HM.fromList $ zip depVarsOrdered perDepVar
         , _srProbability = calcDensity means errs searchDepVarsCoords
         }
    where
        calcDensity :: [Double] -> [Double] -> [Double] -> Double
        calcDensity means errs searchDepVarsCoords
            | any isNaN means = 0/0 -- creates NaN
            | any isNaN errs  = 0/0
            | otherwise       = dnormMulti means errs searchDepVarsCoords

meansAndWeightsOneObs :: [Double] -> KernelDefinition -> [DepVarName] -> ObsWithDist -> Either LOCESTException Double
meansAndWeightsOneObs searchDepVarsCoords kernelDefinition depVars oneObsWithDist = do
    (means, weights) <- unzip <$> mapM (\x -> meanAndWeightOneDepVarOneObs kernelDefinition x oneObsWithDist) depVars
    return $ dnormMulti means weights searchDepVarsCoords

smoothedValueOneDepVar :: KernelDefinition -> [ObsWithDist] -> DepVarName -> Either LOCESTException (Double, Double, Double, Double)
smoothedValueOneDepVar kernelDefinition obsWithDist depVar = do
    (means, weights) <- unzip <$> mapM (meanAndWeightOneDepVarOneObs kernelDefinition depVar) obsWithDist 
    let mean = weightedAvg means weights
        err  = weightedSEM means weights
        density = sum weights
        effn = neff density weights
    return (mean, err, density, effn)

meanAndWeightOneDepVarOneObs :: KernelDefinition -> DepVarName -> ObsWithDist -> Either LOCESTException (Double, Double)
meanAndWeightOneDepVarOneObs kernelDefinition depVar oneObsWithDist = do
    kernel  <- getKernelForOneDepVar kernelDefinition depVar
    mean <- getOneDepVarPos oneObsWithDist
    let weight = weightForOneObs kernel oneObsWithDist
    return (mean, weight)
    where
        getOneDepVarPos :: ObsWithDist -> Either LOCESTException Double
        getOneDepVarPos (ObsWithDist (Observation _ (SpatTempDepVarsPos _ (DepVarsPos m))) _) =
            case HM.lookup depVar m of
                Nothing -> Left $ NormalException "Unknown variable"
                Just x  -> Right x
        weightForOneObs :: Kernel -> ObsWithDist -> Double
        weightForOneObs (Uniform spatRadius tempRadius) (ObsWithDist _ (SpatTempDist spatDist tempDist)) =
            let spatWeight = if spatDist <= spatRadius then 1 else 0
                tempWeight = if tempDist <= tempRadius then 1 else 0
            in spatWeight * tempWeight
        weightForOneObs (Normal spatSigma tempSigma) (ObsWithDist _ (SpatTempDist spatDist tempDist)) =
            dnormMulti [0, 0] [spatSigma ** 2, tempSigma ** 2] [spatDist, tempDist]

getKernelForOneDepVar :: KernelDefinition -> String -> Either LOCESTException Kernel
getKernelForOneDepVar (KernelDefinition kernelsPerDepVar) depVar = do
    case filter (\(KernelOneDepVar n _) -> n == depVar) kernelsPerDepVar of
        []                    -> Left  $ NormalException "Variable not defined in kernel"
        [KernelOneDepVar _ k] -> Right $ k
        _                     -> Left  $ NormalException "Variable defined multiple times in kernel"

filterByDists :: Double -> Double -> [ObsWithDist] -> [ObsWithDist]
filterByDists fs ft = filter (\(ObsWithDist _ (SpatTempDist ds dt)) -> ds <= fs && dt <= ft)

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


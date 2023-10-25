module LocEst.CoreAlgorithms where

import           LocEst.Distance
import           LocEst.MathUtils
import           LocEst.Types
import           LocEst.Utils

import qualified Data.HashMap.Strict as HM
import           Data.List           (unzip4)

coreSearch ::
       [String]
    -> [Observation]
    -> Maybe SpatDistMatrix
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
        (AlgoKernSmooth kernelDefinition)
    ) = do
    -- determine general per-obs statistics
    let searchDepVarsCoords = depVarsExtractOrdered depVarsOrdered searchDepVarPos
    let spatDists = findSpatDistsObsGrid observations maybeSpatDistMap gridSpatTempPos
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
            | otherwise       = dnormMulti means (map sqrt errs) searchDepVarsCoords -- TODO: figure out, why the errs get too small without the sqrt

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
        getOneDepVarPos (ObsWithDist (Observation _ _ (SpatTempDepVarsPos _ (DepVarsPos m))) _) =
            case HM.lookup depVar m of
                Nothing -> Left $ NormalException "Unknown variable"
                Just x  -> Right x
        weightForOneObs :: Kernel -> ObsWithDist -> Double
        weightForOneObs (Uniform spatRadius tempRadius) (ObsWithDist _ (SpatTempDist spatDist tempDist)) =
            let spatWeight = if spatDist <= spatRadius then 1 else 0
                tempWeight = if tempDist <= tempRadius then 1 else 0
            in spatWeight * tempWeight
        weightForOneObs (Normal spatSigma tempSigma) (ObsWithDist _ (SpatTempDist spatDist tempDist)) =
            dnormMulti [0, 0] [spatSigma, tempSigma] [spatDist, tempDist]

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

findSpatDistsObsGrid :: [Observation] -> Maybe SpatDistMatrix -> SpatTempPos -> [Double]
-- calculate distances
findSpatDistsObsGrid observations Nothing gridSpatTempPos =
    map (\x -> spatialDistSpatTempPos gridSpatTempPos . _stpoSpatTempPos . _obsPos $ x) observations
-- look up distances
findSpatDistsObsGrid observations (Just spatDistMatrix) gridSpatTempPos =
    let obsIndizes = map getIndex observations
        gridSpatPosIndex = getIndex $ _spatialPos gridSpatTempPos
    in map (lookUpDistance spatDistMatrix gridSpatPosIndex) obsIndizes


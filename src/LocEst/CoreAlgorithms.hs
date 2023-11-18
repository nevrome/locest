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
        algorithm
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
    perDepVar <- mapM (interpolateOneDepVar algorithm filteredObsWithDists) depVarsOrdered
    let (means, errs) = unzip perDepVar
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

interpolateOneDepVar :: LocestAlgorithm -> [ObsWithDist] -> DepVarName -> Either LOCESTException (Double, Double)
interpolateOneDepVar (AlgoDiffusion kernelDefinition) obsWithDist depVar = do
    kernel <- getKernelForOneDepVar kernelDefinition depVar
    vals <- mapM (valOneDepVarOneObs depVar) obsWithDist
    let weights = map (weightOneObs kernel) obsWithDist
        mean = weightedAvg vals weights
        var = 1 / sum weights
        --err  = weightedSEM vals weights
    return (mean, var)
    where
        weightOneObs :: Kernel -> ObsWithDist -> Double
        weightOneObs kernel@(Kernel _ _ n) oneObsWithDist =
            let d = scaledDistance kernel oneObsWithDist
            in 1 / ((d**2) + n)
interpolateOneDepVar (AlgoKernelSmoothing kernelDefinition) obsWithDist depVar = do
    kernel@(Kernel _ _ n) <- getKernelForOneDepVar kernelDefinition depVar
    vals <- mapM (valOneDepVarOneObs depVar) obsWithDist
    let weights = map (weightOneObs kernel) obsWithDist
        mean = weightedAvg vals weights
        var = n / sum weights
        --err  = weightedSEM vals weights
    return (mean, var)
    where
        weightOneObs :: Kernel -> ObsWithDist -> Double
        weightOneObs kernel@(Kernel _ _ n) oneObsWithDist =
            let d = scaledDistance kernel oneObsWithDist
            in exp (-(d**2))

valOneDepVarOneObs :: DepVarName -> ObsWithDist -> Either LOCESTException Double
valOneDepVarOneObs depVar (ObsWithDist (Observation _ _ (SpatTempDepVarsPos _ (DepVarsPos m))) _) =
    case HM.lookup depVar m of
        Nothing -> Left $ NormalException "Unknown variable"
        Just x  -> Right x

scaledDistance :: Kernel -> ObsWithDist -> Double
scaledDistance (Kernel ss ts _) (ObsWithDist _ (SpatTempDist spatDist tempDist)) =
    --error $ show (ss, ts,  spatDist, tempDist)
    sqrt (((ss * spatDist)**2) + ((ts * tempDist)**2))

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


module LocEst.CoreAlgorithms where

import           LocEst.Distance
import           LocEst.MathUtils
import           LocEst.Types
import           LocEst.Utils

import qualified Control.Monad.Except as E
import           Data.List            (foldl', unzip4)

type CoreLog = E.Except LOCESTException

coreSearch :: [Observation] -> CoreSupplement -> CorePermutation -> CoreLog SearchResult
coreSearch
    observations
    (CoreSupplement spaceTimeFilter maybeSpatDistMap maybeTempSamples)
    searchSetting@(CorePermutation
        (HyperPos searchIndepVarPos searchDepVarPos)
        (AlgoKernSmooth kernelDefinition)
        tempSampIteration
    ) = do
    -- determine dist per obs to current point
    obsWithDist <- case searchIndepVarPos of
        IndepSpatTempPos gridSpatTempPos -> do
            let spatDists = findSpatDistsObsGrid observations maybeSpatDistMap gridSpatTempPos
                spatDistsKM = map (/ 1000) spatDists
                tempDists   = findTempDistsObsGrid observations maybeTempSamples tempSampIteration gridSpatTempPos
                obsRaw = zip3 observations spatDistsKM tempDists
            -- filter by dist (for performance)
            filteredObsWithDists <- case spaceTimeFilter of
                Just (spaceFilter,timeFilter) -> do
                    let res = filter (\(_, ds, dt) -> ds <= spaceFilter && dt <= timeFilter) obsRaw
                    if length res < 3
                    then E.throwError $ NormalException "Less than 3 individuals in subset."
                    else pure res
                Nothing -> pure obsRaw
            return $ map (\(o, s, t) -> ObsWithDist o (IndepSpatTempDist (SpatTempDist s t))) filteredObsWithDists
        IndepArbitraryDimPos arbitraryDimPos -> do
            let arbitraryDimDist = findArbitraryDimDistsObsGrid observations arbitraryDimPos
            return $ zipWith
                (\o d -> ObsWithDist o (IndepArbitraryDimDist d))
                observations arbitraryDimDist
    -- summarize obs information for each depVar
    let searchDepVarsNames  = getKeys searchDepVarPos
        searchDepVarsCoords = getValues searchDepVarPos
    perDepVar <- mapM (smoothedValueOneDepVar kernelDefinition obsWithDist) searchDepVarsNames
    let (means, errs, _, _) = unzip4 perDepVar
    return $ SearchResult {
           _srCorePermutation = searchSetting
         , _srInterpolation = Just $ DepVarsUncertainPos $ zip searchDepVarsNames perDepVar
         , _srProbability = calcDensity means errs searchDepVarsCoords
         }
    where
        calcDensity :: [Double] -> [Double] -> [Double] -> Double
        calcDensity means errs searchDepVarsCoords
            | any isNaN means = 0/0 -- creates NaN
            | any isNaN errs  = 0/0
            | otherwise       = dnormMulti means (map sqrt errs) searchDepVarsCoords -- TODO: figure out, why the errs get too small without the sqrt

smoothedValueOneDepVar :: KernelDefinition -> [ObsWithDist] -> DepVarName -> CoreLog (Double, Double, Double, Double)
smoothedValueOneDepVar kernelDefinition obsWithDist depVar = do
    (means, weights) <- unzip <$> mapM (meanAndWeightOneDepVarOneObs kernelDefinition depVar) obsWithDist
    let mean = weightedAvg means weights
        err  = weightedSEM means weights
        density = sum weights
        effn = neff density weights
    return (mean, err, density, effn)

meanAndWeightOneDepVarOneObs :: KernelDefinition -> DepVarName -> ObsWithDist -> CoreLog (Double, Double)
meanAndWeightOneDepVarOneObs kernelDefinition depVar oneObsWithDist = do
    kernel  <- getKernelForOneDepVar kernelDefinition depVar
    mean    <- getOneDepVarPos oneObsWithDist
    let weight = weightForOneObs kernel oneObsWithDist
    return (mean, weight)
    where
        getOneDepVarPos :: ObsWithDist -> CoreLog Double
        getOneDepVarPos (ObsWithDist (Observation _ _ (HyperPos _ (DepVarsPos m))) _) =
            case lookup depVar m of
                Nothing -> E.throwError $ NormalException "Unknown variable"
                Just x  -> pure x
        weightForOneObs :: Kernel -> ObsWithDist -> Double
        -- uniform kernel
        weightForOneObs (Uniform [spatRadius, tempRadius])
                        (ObsWithDist _ (IndepSpatTempDist (SpatTempDist spatDist tempDist))) =
            let spatWeight = if spatDist <= spatRadius then 1 else 0
                tempWeight = if tempDist <= tempRadius then 1 else 0
            in spatWeight * tempWeight
        weightForOneObs (Uniform radii)
                        (ObsWithDist _ (IndepArbitraryDimDist ds)) =
            let inRadii = zipWith (\radius d -> if d <= radius then 1 else 0) radii ds
            in foldl' (*) 1 inRadii
        -- gaussian kernel
        weightForOneObs (Normal [spatSigma, tempSigma])
                        (ObsWithDist _ (IndepSpatTempDist (SpatTempDist spatDist tempDist))) =
            dnormMulti [0, 0] [spatSigma, tempSigma] [spatDist, tempDist]
        weightForOneObs (Normal sigmas)
                        (ObsWithDist _ (IndepArbitraryDimDist ds)) =
            dnormMulti (repeat 0) sigmas ds
        weightForOneObs _ _ = error "this should never happen"

getKernelForOneDepVar :: KernelDefinition -> String -> CoreLog Kernel
getKernelForOneDepVar (KernelDefinition kernelsPerDepVar) depVar = do
    case filter (\(KernelOneDepVar n _) -> n == depVar) kernelsPerDepVar of
        []                    -> E.throwError $ NormalException "Variable not defined in kernel"
        [KernelOneDepVar _ k] -> pure k
        _                     -> E.throwError $ NormalException "Variable defined multiple times in kernel"

findTempDistsObsGrid :: [Observation] -> Maybe TempSampleMatrix -> Int -> SpatTempPos -> [Double]
-- calculate distances from mean ages
findTempDistsObsGrid observations Nothing _ gridSpatTempPos =
    let spatTempPos = map (extractSpatTempPos . _hyposIndepVarsPos . _obsPos) observations
    in map (temporalDistSpatTempPos gridSpatTempPos) spatTempPos
-- look up age samples and calculate distances from them
findTempDistsObsGrid observations (Just tempSampleMatrix) iteration gridSpatTempPos =
    let obsIndizes = map getIndex observations
        obsAgeSamples = map (lookUpTempSample tempSampleMatrix iteration) obsIndizes
        (SpatTempPos _ (TempPos gridPointAge)) = gridSpatTempPos
    in map (temporalDistYearBCAD gridPointAge) obsAgeSamples

findSpatDistsObsGrid :: [Observation] -> Maybe SpatDistMatrix -> SpatTempPos -> [Double]
-- calculate distances
findSpatDistsObsGrid observations Nothing gridSpatTempPos =
    map (spatialDistSpatTempPos gridSpatTempPos . extractSpatTempPos . _hyposIndepVarsPos . _obsPos) observations
-- look up distances
findSpatDistsObsGrid observations (Just spatDistMatrix) gridSpatTempPos =
    let obsIndizes = map getIndex observations
        gridSpatPosIndex = getIndex $ _spatialPos gridSpatTempPos
    in map (lookUpDistance spatDistMatrix gridSpatPosIndex) obsIndizes

findArbitraryDimDistsObsGrid :: [Observation] -> ArbitraryDimPos -> [[Double]]
findArbitraryDimDistsObsGrid observations gridAbritryDimPos =
    let gridPos = getValues gridAbritryDimPos
    in map (allDistances gridPos . getValues . extractArbitraryDimPos . _hyposIndepVarsPos . _obsPos) observations

extractSpatTempPos :: IndepVarsPos -> SpatTempPos
extractSpatTempPos (IndepSpatTempPos x) = x
extractSpatTempPos _                    = error "this should never happen"

extractArbitraryDimPos :: IndepVarsPos -> ArbitraryDimPos
extractArbitraryDimPos (IndepArbitraryDimPos x) = x
extractArbitraryDimPos _                        = error "this should never happen"

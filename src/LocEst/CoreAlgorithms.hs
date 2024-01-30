module LocEst.CoreAlgorithms where

import           LocEst.Distance
import           LocEst.MathUtils
import           LocEst.Types
import           LocEst.Utils

import qualified Control.Monad.Except as E
import           Data.List            (foldl', unzip4)

type CoreLog = E.Except LOCESTException

getDist :: [Observation] -> CoreSupplement -> CorePermutation -> [ObsWithDist]
getDist [] _ _ = []
getDist
    (obs@(Observation obsIndex _ (HyperPos (IndepSpatTempPos obsSpatTempPos) _)) : rest)
    s@(CoreSupplement maybeSpaceTimeFilter maybeSpatDistMap maybeTempSamples)
    searchSetting@(CorePermutation (HyperPos (IndepSpatTempPos gridSpatTempPos) _) _ tempSampIteration
    ) = let tempDists = findTempDist maybeTempSamples
            spatDists = findSpatDist maybeSpatDistMap
            spatDistsKM = spatDists/1000
        in ObsWithDist obs (IndepSpatTempDist (SpatTempDist spatDistsKM tempDists)) : getDist rest s searchSetting
        where
            findTempDist :: Maybe TempSampleMatrix -> Double
            -- calculate distances from mean ages
            findTempDist Nothing = temporalDistSpatTempPos gridSpatTempPos obsSpatTempPos
            -- look up age samples and calculate distances from them
            findTempDist (Just tempSampleMatrix) =
                let (SpatTempPos _ (TempPos gridPointAge)) = gridSpatTempPos
                    obsAgeSample = lookUpTempSample tempSampleMatrix tempSampIteration obsIndex
                in temporalDistYearBCAD gridPointAge obsAgeSample
            findSpatDist :: Maybe SpatDistMatrix -> Double
            -- calculate distances
            findSpatDist Nothing = spatialDistSpatTempPos gridSpatTempPos obsSpatTempPos
            -- look up distances
            findSpatDist (Just spatDistMatrix) =
                let gridSpatPosIndex = getIndex $ _spatialPos gridSpatTempPos
                in lookUpDistance spatDistMatrix gridSpatPosIndex obsIndex
getDist (_ : rest) s searchSetting = getDist rest s searchSetting

coreSearch :: [Observation] -> CoreSupplement -> CorePermutation -> CoreLog SearchResult
coreSearch
    observations
    (CoreSupplement maybeSpaceTimeFilter maybeSpatDistMap maybeTempSamples)
    searchSetting@(CorePermutation
        (HyperPos searchIndepVarPos searchDepVarPos)
        (AlgoKernSmooth kernelDefinition)
        tempSampIteration
    ) = do
    -- determine dist per obs to current point
    obsWithDist <- case searchIndepVarPos of
        IndepSpatTempPos gridSpatTempPos -> do
            spatDists <- findSpatDistsObsGrid observations maybeSpatDistMap gridSpatTempPos
            tempDists <- findTempDistsObsGrid observations maybeTempSamples tempSampIteration gridSpatTempPos
            let spatDistsKM = map (/ 1000) spatDists
                obsRaw = zip3 observations spatDistsKM tempDists
            -- filter by dist (for performance)
            filteredObsWithDists <- case maybeSpaceTimeFilter of
                Just (spaceFilter,timeFilter) -> do
                    let res = filter (\(_, ds, dt) -> ds <= spaceFilter && dt <= timeFilter) obsRaw
                    if length res < 3
                    then E.throwError $ NormalException "Less than 3 individuals in subset."
                    else pure res
                Nothing -> pure obsRaw
            return $ map (\(o, s, t) -> ObsWithDist o (IndepSpatTempDist (SpatTempDist s t))) filteredObsWithDists
        IndepArbitraryDimPos arbitraryDimPos -> do
            arbitraryDimDist <- findArbitraryDimDistsObsGrid observations arbitraryDimPos
            return $ zipWith
                (\o d -> ObsWithDist o (IndepArbitraryDimDist d))
                observations arbitraryDimDist
    -- summarize obs information for each depVar
    let searchDepVarsNames  = getKeys searchDepVarPos
        searchDepVarsCoords = getValues searchDepVarPos
    perDepVar <- mapM (smoothedValueOneDepVar kernelDefinition obsWithDist) searchDepVarsNames
    let (means, errs, density, _) = unzip4 perDepVar
    return $ SearchResult {
           _srCorePermutation = searchSetting
         , _srInterpolation = Just $ DepVarsUncertainPos $ zip searchDepVarsNames perDepVar
         --, _srProbability = calcDensity means errs searchDepVarsCoords
         -- hacky rescaling of the probability with the density
         , _srProbability = ((minimum density) ** (1/4)) * calcDensity means errs searchDepVarsCoords
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
    kernel <- getKernelForOneDepVar kernelDefinition depVar
    mean   <- getOneDepVarPos oneObsWithDist
    weight <- weightForOneObs kernel oneObsWithDist
    return (mean, weight)
    where
        getOneDepVarPos :: ObsWithDist -> CoreLog Double
        getOneDepVarPos (ObsWithDist (Observation _ _ (HyperPos _ (DepVarsPos m))) _) =
            case lookup depVar m of
                Nothing -> E.throwError $ NormalException "Unknown variable"
                Just x  -> pure x
        weightForOneObs :: Kernel -> ObsWithDist -> CoreLog Double
        -- uniform kernel
        weightForOneObs (Uniform [(_,spatRadius), (_,tempRadius)])
                        (ObsWithDist _ (IndepSpatTempDist (SpatTempDist spatDist tempDist))) =
            let spatWeight = if spatDist <= spatRadius then 1 else 0
                tempWeight = if tempDist <= tempRadius then 1 else 0
            in pure $ spatWeight * tempWeight
        weightForOneObs u@(Uniform _)
                        (ObsWithDist _ (IndepArbitraryDimDist ds)) = do
            let inRadii = zipWith (\radius d -> if d <= radius then 1 else 0) (getValues u) ds
            pure $ foldl' (*) 1 inRadii
        -- gaussian kernel
        weightForOneObs (Normal [(_,spatSigma), (_,tempSigma)])
                        (ObsWithDist _ (IndepSpatTempDist (SpatTempDist spatDist tempDist))) =
            pure $ dnormMulti [0, 0] [spatSigma, tempSigma] [spatDist, tempDist]
        weightForOneObs n@(Normal _)
                        (ObsWithDist _ (IndepArbitraryDimDist ds)) =
            pure $ dnormMulti (repeat 0) (getValues n) ds
        -- mismatch error case
        weightForOneObs _ _ =
            E.throwError $ NormalException "Illegal combination of kernel and grid data"

getKernelForOneDepVar :: KernelDefinition -> String -> CoreLog Kernel
getKernelForOneDepVar (KernelDefinition kernelsPerDepVar) depVar = do
    case filter (\(KernelOneDepVar n _) -> n == depVar) kernelsPerDepVar of
        []                    -> E.throwError $ NormalException "Variable not defined in kernel"
        [KernelOneDepVar _ k] -> pure k
        _                     -> E.throwError $ NormalException "Variable defined multiple times in kernel"

findTempDistsObsGrid :: [Observation] -> Maybe TempSampleMatrix -> Int -> SpatTempPos -> CoreLog [Double]
-- calculate distances from mean ages
findTempDistsObsGrid observations Nothing _ gridSpatTempPos = do
    spatTempPos <- mapM (extractSpatTempPos . _hyposIndepVarsPos . _obsPos) observations
    return $ map (temporalDistSpatTempPos gridSpatTempPos) spatTempPos
-- look up age samples and calculate distances from them
findTempDistsObsGrid observations (Just tempSampleMatrix) iteration gridSpatTempPos = do
    let obsIndizes = map getIndex observations
        obsAgeSamples = map (lookUpTempSample tempSampleMatrix iteration) obsIndizes
        (SpatTempPos _ (TempPos gridPointAge)) = gridSpatTempPos
    return $ map (temporalDistYearBCAD gridPointAge) obsAgeSamples

findSpatDistsObsGrid :: [Observation] -> Maybe SpatDistMatrix -> SpatTempPos -> CoreLog [Double]
-- calculate distances
findSpatDistsObsGrid observations Nothing gridSpatTempPos = do
    spatTempPos <- mapM (extractSpatTempPos . _hyposIndepVarsPos . _obsPos) observations
    return $ map (spatialDistSpatTempPos gridSpatTempPos) spatTempPos
-- look up distances
findSpatDistsObsGrid observations (Just spatDistMatrix) gridSpatTempPos = do
    let obsIndizes = map getIndex observations
        gridSpatPosIndex = getIndex $ _spatialPos gridSpatTempPos
    return $ map (lookUpDistance spatDistMatrix gridSpatPosIndex) obsIndizes

findArbitraryDimDistsObsGrid :: [Observation] -> ArbitraryDimPos -> CoreLog [[Double]]
findArbitraryDimDistsObsGrid observations gridAbritryDimPos = do
    let gridPos = getValues gridAbritryDimPos
    arbitraryDimPos <- mapM (extractArbitraryDimPos . _hyposIndepVarsPos . _obsPos) observations
    return $ map (allDistances gridPos . getValues) arbitraryDimPos

extractSpatTempPos :: IndepVarsPos -> CoreLog SpatTempPos
extractSpatTempPos (IndepSpatTempPos x) = pure x
extractSpatTempPos _                    = E.throwError $ NormalException "this should never happen 1"

extractArbitraryDimPos :: IndepVarsPos -> CoreLog ArbitraryDimPos
extractArbitraryDimPos (IndepArbitraryDimPos x) = pure x
extractArbitraryDimPos _                        = E.throwError $ NormalException "this should never happen 2"

module LocEst.CoreAlgorithms where

import           LocEst.Distance
import           LocEst.MathUtils
import           LocEst.Types
import           LocEst.Utils

import qualified Control.Monad.Except as E
import           Data.List            (foldl', unzip4)

type CoreLog = E.Except LOCESTException

coreSearch :: [Observation] -> CoreSupplement -> CorePermutation -> CoreLog SearchResult
coreSearch observations supp
    sett@(CorePermutation (HyperPos _ searchDepVarPos) (AlgoKernSmooth kernelDefinition) _) = do
    -- determine dist per obs to current point
    let obsWithDist = getDist observations supp sett
    -- summarize obs information for each depVar
    let searchDepVarsNames  = getKeys searchDepVarPos
        searchDepVarsCoords = getValues searchDepVarPos
    perDepVar <- mapM (smoothedValueOneDepVar kernelDefinition obsWithDist) searchDepVarsNames
    let (means, errs, density, _) = unzip4 perDepVar
        -- probability = calcDensity means errs searchDepVarsCoords
        -- hacky rescaling of the probability with the density
        probability = ((minimum density) ** (1/4)) * calcDensity means errs searchDepVarsCoords
    return $ SearchResult {
           _srCorePermutation = sett
         , _srInterpolation = Just $ DepVarsUncertainPos $ zip searchDepVarsNames perDepVar
         , _srProbability = probability
         }
    where
        calcDensity :: [Double] -> [Double] -> [Double] -> Double
        calcDensity means errs searchDepVarsCoords
            | any isNaN means = 0/0 -- creates NaN
            | any isNaN errs  = 0/0
            | otherwise       = dnormMulti means (map sqrt errs) searchDepVarsCoords -- TODO: figure out, why the errs get too small without the sqrt

getDist :: [Observation] -> CoreSupplement -> CorePermutation -> [ObsWithDist]
getDist [] _ _ = []
-- spatiotemporal distances
getDist
    (obs@(Observation obsIndex _ (HyperPos (IndepSpatTempPos obsSpatTempPos) _)) : rest)
    supp@(CoreSupplement maybeSpaceTimeFilter maybeSpatDistMap maybeTempSamples)
    sett@(CorePermutation (HyperPos (IndepSpatTempPos gridSpatTempPos) _) _ tempSampIteration) =
        let tempDist = findTempDist maybeTempSamples
            spatDist = findSpatDist maybeSpatDistMap
            spatDistsKM = spatDist/1000
            filtered = case maybeSpaceTimeFilter of
                Just (spaceFilter,timeFilter) -> spatDistsKM <= spaceFilter && tempDist <= timeFilter
                Nothing -> False
        in if filtered
           then getDist rest supp sett
           else ObsWithDist obs (IndepSpatTempDist (SpatTempDist spatDistsKM tempDist)) : getDist rest supp sett
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
-- arbitrary dim distances
getDist
    (obs@(Observation _ _ (HyperPos (IndepArbitraryDimPos obsArbitraryDimPos) _)) : rest)
    supp
    sett@(CorePermutation (HyperPos (IndepArbitraryDimPos gridAbritryDimPos) _) _ _) =
        let arbitraryDimDist = findArbitraryDimDistsObsGrid
        in ObsWithDist obs (IndepArbitraryDimDist arbitraryDimDist) : getDist rest supp sett
        where
            findArbitraryDimDistsObsGrid :: [Double]
            findArbitraryDimDistsObsGrid =
                let obsPos = getValues obsArbitraryDimPos
                    gridPos = getValues gridAbritryDimPos
                in allDistances obsPos gridPos
-- wrong input, so skip
getDist (_ : rest) supp sett = getDist rest supp sett

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

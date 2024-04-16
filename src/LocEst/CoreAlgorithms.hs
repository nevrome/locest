module LocEst.CoreAlgorithms where

import           LocEst.Distance
import           LocEst.MathUtils
import           LocEst.Types
import           LocEst.Utils

import           Control.Monad           (mapAndUnzipM)
import qualified Control.Monad.Except    as E
import           Data.Maybe              (mapMaybe)
import           Statistics.Distribution (density, quantile)

type CoreLog = E.Except LOCESTException

coreSearch :: [Observation] -> CoreSupplement -> CorePermutation -> CoreLog SearchResult
coreSearch observations supp
    sett@(CorePermutation _ searchDepVarPos (AlgoKernSmooth kernelDefinition) _) = do
    -- determine distances per observation to the current position of interest
    let obsWithDist = getDist observations supp sett
    -- determine (interpolated) posterior predictive distributions per depVar for this position,
    -- derive summary statistics and maybe perform the search for a specific search depVar value
    let namePerDepVar  = getKeys kernelDefinition
        valuePerDepVar = case searchDepVarPos of
            Just x  -> Just <$> getValues x
            Nothing -> replicate (length namePerDepVar) Nothing
    interpolPerDepVar <- mapM (interpolAndSearchOneDepVar kernelDefinition obsWithDist) $ zip namePerDepVar valuePerDepVar
    -- compile output object
    return $ SearchResult {
           _srCorePermutation = sett
         , _srInterpolation   = InterpolationResult interpolPerDepVar
         , _srProbability     = case mapMaybe _irodvProbability interpolPerDepVar of
            [] -> Nothing
            xs -> Just $ foldSum xs
         }

getDist :: [Observation] -> CoreSupplement -> CorePermutation -> [ObsWithDist]
getDist [] _ _ = []
-- spatiotemporal distances
getDist
    (obs@(Observation obsIndex _ (HyperPos (IndepSpatTempPos obsSpatTempPos) _)) : rest)
    supp@(CoreSupplement maybeSpaceTimeFilter maybeSpatDistMap maybeTempSamples)
    sett@(CorePermutation (IndepSpatTempPos gridSpatTempPos) _ _ tempSampIteration) =
        let tempDist = findTempDist maybeTempSamples
            spatDist = findSpatDist maybeSpatDistMap
            spatDistsKM = spatDist/1000
            filtered = case maybeSpaceTimeFilter of
                Just (spaceFilter,timeFilter) -> spatDistsKM > spaceFilter || tempDist > timeFilter
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
                in lookUpDistanceAU spatDistMatrix gridSpatPosIndex obsIndex
-- arbitrary dim distances
getDist
    (obs@(Observation _ _ (HyperPos (IndepArbitraryDimPos obsArbitraryDimPos) _)) : rest)
    supp
    sett@(CorePermutation (IndepArbitraryDimPos gridAbritryDimPos) _ _ _) =
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

interpolAndSearchOneDepVar :: KernelDefinition -> [ObsWithDist] -> (DepVarName, Maybe Double) -> CoreLog InterpolationResultOneDepVar
interpolAndSearchOneDepVar kernelDefinition obsWithDist (nameDepVar,maybeValueDepVar) = do
    (values, weights) <- mapAndUnzipM (valueAndWeightOneDepVarOneObs kernelDefinition nameDepVar) obsWithDist
    let totalWeight = foldSum weights
        neff        = totalWeight
        weightedA   = weightedAvg_ totalWeight values weights
        weightedV   = weightedVar_ totalWeight weightedA values weights
    case posteriorPredictive_ totalWeight weightedA weightedV of
        Right distribution -> do
            let lower  = quantile distribution 0.025
                median = quantile distribution 0.5
                upper  = quantile distribution 0.975
                prob   = fmap (density distribution) maybeValueDepVar
            return $ InterpolationResultOneDepVar nameDepVar neff weightedA weightedV lower median upper prob
        Left e -> do
            -- probably doesn't work because of normalization:
            --return $ InterpolationResultOneDepVar nameDepVar neff weightedA weightedV (0/0) (0/0) (0/0) (0/0)
            E.throwError $ NormalException e

valueAndWeightOneDepVarOneObs :: KernelDefinition -> DepVarName -> ObsWithDist -> CoreLog (Double, Double)
valueAndWeightOneDepVarOneObs kernelDefinition depVar oneObsWithDist = do
    (nugget,kernel) <- getKernelForOneDepVar kernelDefinition depVar
    value  <- getOneDepVarPos oneObsWithDist
    weight <- weightForOneObs nugget kernel oneObsWithDist
    return (value, weight)
    where
        getOneDepVarPos :: ObsWithDist -> CoreLog Double
        getOneDepVarPos (ObsWithDist (Observation _ _ (HyperPos _ (DepVarsPos m))) _) =
            case lookup depVar m of
                Nothing -> E.throwError $ NormalException "Unknown variable"
                Just x  -> pure x
        weightForOneObs :: Nugget -> Kernel -> ObsWithDist -> CoreLog Double
        -- squared-exponential kernel
        weightForOneObs nugget
                        (SquaredExponential (ArbitraryDimPos [(_,spaceKernelWidth), (_,timeKernelWidth)]))
                        (ObsWithDist _ (IndepSpatTempDist (SpatTempDist spatDist tempDist))) =
            pure $ nugget / (nugget + exp ( (spatDist / spaceKernelWidth) ** 2 + (tempDist / timeKernelWidth) ** 2) - 1)
        weightForOneObs nugget
                        kernel
                        (ObsWithDist _ (IndepArbitraryDimDist ds)) =
            pure $ nugget / (nugget + exp ( foldSum (zipWith (\d t -> (d / t) ** 2) ds (getValues kernel)) ) - 1)
        -- mismatch error case
        weightForOneObs _ _ _ =
            E.throwError $ NormalException "Illegal combination of kernel and grid data"

getKernelForOneDepVar :: KernelDefinition -> String -> CoreLog (Nugget, Kernel)
getKernelForOneDepVar (KernelDefinition kernelsPerDepVar) depVar = do
    case filter (\(KernelOneDepVar name _ _) -> name == depVar) kernelsPerDepVar of
        []                    -> E.throwError $ NormalException "Variable not defined in kernel"
        [KernelOneDepVar _ n k] -> pure (n, k)
        _                     -> E.throwError $ NormalException "Variable defined multiple times in kernel"

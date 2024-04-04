module LocEst.CoreAlgorithms where

import           LocEst.Distance
import           LocEst.MathUtils
import           LocEst.Types
import           LocEst.Utils

import qualified Control.Monad.Except as E
import Statistics.Distribution.StudentT (StudentT)
import Statistics.Distribution (quantile, density)
import Statistics.Distribution.Transform (LinearTransform)

type CoreLog = E.Except LOCESTException

coreSearch :: [Observation] -> CoreSupplement -> CorePermutation -> CoreLog SearchResult
coreSearch observations supp
    sett@(CorePermutation (HyperPos _ searchDepVarPos) (AlgoKernSmooth kernelDefinition) _) = do
    -- determine distances per observation to the current position of interest
    let obsWithDist = getDist observations supp sett
    -- determine (interpolated) posterior predictive distributions per depVar for this position
    let searchDepVarsNames = getKeys searchDepVarPos
    interpolDistributionPerDepVar <- mapM (interpolOneDepVar kernelDefinition obsWithDist) searchDepVarsNames
    -- determine interpolation output quantities of interpolation for each depVar
    let interpolDerived = map summarizeInterpol interpolDistributionPerDepVar
        interpolRes = InterpolationResult $ zip searchDepVarsNames interpolDerived
    -- perform search
    let searchDepVarsValues = getValues searchDepVarPos
        probability = foldSum $ zipWith density interpolDistributionPerDepVar searchDepVarsValues
    -- compile output object
    return $ SearchResult {
           _srCorePermutation = sett
         , _srInterpolation   = Just interpolRes
         , _srProbability     = probability
         }
    where
        summarizeInterpol :: LinearTransform StudentT -> (Double, Double, Double)
        summarizeInterpol distribution =
            let lower  = quantile distribution 0.025
                median = quantile distribution 0.5
                upper  = quantile distribution 0.975
            in (lower, median, upper)

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

interpolOneDepVar :: KernelDefinition -> [ObsWithDist] -> DepVarName -> CoreLog (LinearTransform StudentT)
interpolOneDepVar kernelDefinition obsWithDist depVar = do
    (values, weights) <- unzip <$> mapM (valueAndWeightOneDepVarOneObs kernelDefinition depVar) obsWithDist
    return $ posteriorPredictive values weights

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
                        (SquaredExponential [(_,spaceKernelWidth), (_,timeKernelWidth)])
                        (ObsWithDist _ (IndepSpatTempDist (SpatTempDist spatDist tempDist))) =
            pure $ nugget / (nugget + exp ( (spatDist ** 2) / spaceKernelWidth + (tempDist ** 2) / timeKernelWidth ) - 1)
        weightForOneObs nugget
                        kernel
                        (ObsWithDist _ (IndepArbitraryDimDist ds)) =
            pure $ nugget / (nugget + exp ( foldSum (zipWith (\d t -> d ** 2 / t) ds (getValues kernel)) ) - 1)
        -- mismatch error case
        weightForOneObs _ _ _ =
            E.throwError $ NormalException "Illegal combination of kernel and grid data"

getKernelForOneDepVar :: KernelDefinition -> String -> CoreLog (Nugget, Kernel)
getKernelForOneDepVar (KernelDefinition kernelsPerDepVar) depVar = do
    case filter (\(KernelOneDepVar name _ _) -> name == depVar) kernelsPerDepVar of
        []                    -> E.throwError $ NormalException "Variable not defined in kernel"
        [KernelOneDepVar _ n k] -> pure (n, k)
        _                     -> E.throwError $ NormalException "Variable defined multiple times in kernel"

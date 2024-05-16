module LocEst.CoreAlgorithms where

import           LocEst.Distance
import           LocEst.MathUtils
import           LocEst.Types
import           LocEst.Utils

import qualified Control.Monad.Except    as E
import           Data.Maybe              (mapMaybe)
import           Statistics.Distribution (density, quantile)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

type CoreLog = E.Except LOCESTException
-- you could throw a clean exception with for just one core iteration with
-- E.throwError $ NormalException ""

data CoreOutMode =
      CoreOutShort
    | CoreOutFull
    | CoreOutObsWeight Int

core ::
       CoreOutMode
    -> CoreSupplement
    -> V.Vector Observation
    -> CorePermutation
    -> CoreLog CoreOut
core (CoreOutObsWeight nrTopObs) supp observations sett@(CorePermutation _ _ kernelDefinition _) = do
    --let namePerDepVar  = getKeys kernelDefinition
    --obsWithWeights <- V.mapMaybeM (getWeightsPerObs supp sett namePerDepVar) observations
    --return $ CoreObsWeight (V.map (ObsWeight sett) obsWithWeights)
    undefined
core
    outMode 
    (CoreSupplement maybeSpaceTimeFiler maybeSpatDistMap maybeTempSamples)
     observations sett@(CorePermutation _ searchDepVarPos kernelDefinition _) = do
    let dists = V.map (getDists maybeSpatDistMap maybeTempSamples sett) observations
        obsWithDist = V.filter (inFilterRange maybeSpaceTimeFiler) $ V.zip observations dists
    let namePerDepVar  = getKeys kernelDefinition
        valuePerDepVar = case searchDepVarPos of
            Just x  -> Just <$> getValues x
            Nothing -> replicate (length namePerDepVar) Nothing
        interpolPerDepVarFull = zipWith (interpolAndSearchOneDepVar kernelDefinition obsWithDist) namePerDepVar valuePerDepVar
        interpolPerDepVar = case outMode of
            CoreOutShort -> map resOneDepvar2Short interpolPerDepVarFull
            CoreOutFull -> interpolPerDepVarFull
    return $ CoreSearchResult $ SearchResult {
           _srCorePermutation = sett
         , _srInterpolation   = InterpolationResult interpolPerDepVar
         , _srProbability     = case mapMaybe getProbability interpolPerDepVarFull of
            [] -> Nothing
            xs -> Just $ foldSum xs
         }

getDists ::
       Maybe SpatDistMatrix
    -> Maybe TempSampleMatrix
    -> CorePermutation
    -> Observation
    -> IndepVarsDist
-- spatiotemporal distances
getDists
    maybeSpatDistMap maybeTempSamples
    (CorePermutation (IndepSpatTempPos gridSpatTempPos) _ _ tempSampIteration)
    obs@(Observation obsIndex _ (HyperPos (IndepSpatTempPos obsSpatTempPos) _)) =
        let spatDist = findSpatDist maybeSpatDistMap
            spatDistsKM = spatDist/1000
            tempDist = findTempDist maybeTempSamples  
        in IndepSpatTempDist (SpatTempDist spatDistsKM tempDist)
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
getDists
    _ _
    (CorePermutation (IndepArbitraryDimPos gridAbritryDimPos) _ _ _)
    obs@(Observation _ _ (HyperPos (IndepArbitraryDimPos obsArbitraryDimPos) _)) =
        let keys = getKeys obsArbitraryDimPos
            obsPos = getValues obsArbitraryDimPos
            gridPos = getValues gridAbritryDimPos
            arbitraryDimDist = ArbitraryDimPos $ zip keys (allDistances obsPos gridPos)
        in IndepArbitraryDimDist arbitraryDimDist
-- wrong input
getDists _ _ _ _ = error "Should not happen" -- ToDo

inFilterRange :: Maybe (Double, Double) -> (Observation, IndepVarsDist) -> Bool
inFilterRange
    (Just (spaceFilter,timeFilter))
    (_,IndepSpatTempDist (SpatTempDist spatDistsKM tempDist)) =
    spatDistsKM <= spaceFilter && tempDist <= timeFilter
inFilterRange _ _ = True

interpolAndSearchOneDepVar ::
       KernelDefinition
    -> V.Vector (Observation, IndepVarsDist)
    -> DepVarName
    -> Maybe Double
    -> InterpolationResultOneDepVar
interpolAndSearchOneDepVar kernelDefinition obsWithDist nameDepVar maybeValueDepVar = do
    let (values, weights) = VU.unzip . VU.convert $ V.map (valueAndWeightOneDepVarOneObs kernelDefinition nameDepVar) obsWithDist
        totalWeight = VU.sum weights
        neff        = totalWeight
        weightedA   = weightedAvg_ totalWeight values weights
        weightedV   = weightedVar_ totalWeight weightedA values weights
    case posteriorPredictive_ totalWeight weightedA weightedV of
        Right distribution ->
            let lower  = quantile distribution 0.025
                median = quantile distribution 0.5 -- this is identical to weightedA
                upper  = quantile distribution 0.975
                prob   = fmap (density distribution) maybeValueDepVar
            in InterpolationResultOneDepVarFull nameDepVar neff weightedA weightedV (OutBool True) (OutInfDouble lower) median (OutInfDouble upper) prob
        Left _ ->
            case maybeValueDepVar of
                Just _ ->
                    -- is setting the probability to 0 a good idea?
                    InterpolationResultOneDepVarFull nameDepVar neff weightedA weightedV (OutBool False) (OutInfDouble (-infinity)) weightedA (OutInfDouble infinity) (Just 0)
                Nothing ->
                    InterpolationResultOneDepVarFull nameDepVar neff weightedA weightedV (OutBool False) (OutInfDouble (-infinity)) weightedA (OutInfDouble infinity) Nothing

valueAndWeightOneDepVarOneObs ::
       KernelDefinition
    -> DepVarName
    -> (Observation, IndepVarsDist)
    -> (Double, Double)
valueAndWeightOneDepVarOneObs kernelDefinition depVar (obs,dists) =
    let (shape,nugget,lengths) = getKernelForOneDepVar kernelDefinition depVar
        value = getOneDepVarPos obs
        sqWeiDist = squaredWeightedDistForOneObs lengths dists
        weight = weightForOneObs shape nugget sqWeiDist
    in (value, weight)
    where
        getOneDepVarPos :: Observation -> Double
        getOneDepVarPos (Observation _ _ (HyperPos _ (DepVarsPos m))) =
            case lookup depVar m of
                Nothing -> error "Unknown variable"
                Just x  -> x
        weightForOneObs :: KernelShape -> KernelNugget -> Double -> Double
        weightForOneObs SquaredExponential nugget d = nugget / (nugget + exp d - 1)
        weightForOneObs Linear             nugget d = nugget / (nugget + sqrt d)
        squaredWeightedDistForOneObs :: KernelLengths -> IndepVarsDist -> Double
        squaredWeightedDistForOneObs
            (KernelLengths (ArbitraryDimPos [(_,spaceKernelWidth), (_,timeKernelWidth)]))
            (IndepSpatTempDist (SpatTempDist spatDist tempDist)) =
            (spatDist / spaceKernelWidth) ** 2 + (tempDist / timeKernelWidth) ** 2
        squaredWeightedDistForOneObs
            lengths
            (IndepArbitraryDimDist namedDists) =
            let ds = getValues namedDists
            in foldSum (zipWith (\d t -> (d / t) ** 2) ds (getValues lengths))
        squaredWeightedDistForOneObs _ _ =
            error "Illegal combination of kernel and grid data"

getKernelForOneDepVar :: KernelDefinition -> String -> (KernelShape, KernelNugget, KernelLengths)
getKernelForOneDepVar (KernelDefinition kernelsPerDepVar) depVar = do
    case filter (\(KernelOneDepVar name _ _ _) -> name == depVar) kernelsPerDepVar of
        []                        -> error "Variable not defined in kernel definition"
        [KernelOneDepVar _ s n k] -> (s, n, k)
        _                         -> error "Variable defined multiple times in kernel definition"

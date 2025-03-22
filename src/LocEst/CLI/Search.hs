{-# LANGUAGE ScopedTypeVariables #-}

module LocEst.CLI.Search where

import           LocEst.CLI.Utils
import           LocEst.CoreAlgorithms
import           LocEst.Exceptions
import           LocEst.MathUtils
import           LocEst.Parsers
import           LocEst.Types

import           Data.Conduit                  ((.|))
import qualified Data.Conduit                  as Con
import qualified Data.Conduit.Algorithms.Async as ConAA
import qualified Data.Conduit.Combinators      as ConC
import qualified Data.Conduit.List             as ConL
import           Data.Maybe                    (catMaybes)
import qualified Data.Vector                   as V
import qualified Data.Vector.Unboxed           as VU
import           System.FilePath               (takeExtension)
import           System.IO                     (hPutStrLn, stderr)
import           System.Random.Stateful        as R
import LocEst.Distance
import qualified Numeric.LinearAlgebra as M
import Data.List (zip4, zip5)

data SearchOptions = SearchOptions
    { _searchInObservationFile  :: FilePath
    , _searchSearchGridSettings :: SearchGridSettings
    , _searchAlgorithm          :: KernelDefinition
    , _normalise                :: Normalisation
    , _searchOutFile            :: Maybe FilePath
    , _searchOutMode            :: CoreOutMode
    }

data SearchGridSettings = SearchGridSettings {
      _searchPosSetIndepVarsGrid :: IndepVarsPredGridSettings
    , _searchPosSetDepVarsGrid   :: Maybe DepVarsPredGridSettings
}

data DepVarsPredGridSettings =
      DirectDepVarsGridSettings [DepVarsPos]
    | SearchObsDepVarsGridSettings FilePath

data IndepVarsPredGridSettings = SpaceTimeGridSettings {
      _stgsInSpatGridFile     :: FilePath
    , _stgsInTempGrid         :: [AbsRelTempPos]
    , _stgsSupplementSettings :: CoreSupplementSettings
} | ArbitraryDimGridSettings {
      _adgsInArbitraryDimGridFile :: FilePath
    , _adgsSupplementSettings     :: CoreSupplementSettings
}

data CoreSupplementSettings = CoreSupplementSettings {
      _stcsDistFilterThresholds :: Maybe DistanceThresholds
    , _stcsInSpatDistFile       :: Maybe FilePath
    , _stcsInObsTempSamplesFile :: Maybe FilePath
    , _stcsNoOrderCheck         :: Bool
}

runSearch :: SearchOptions -> Int -> Double -> IO ()
runSearch (
    SearchOptions
        inObsFile
        (SearchGridSettings indepVarsPredGridSettings depVarsPredGridSettings)
        kernelDefinition
        normalisation
        outFile
        outMode
    ) numThreads spatDistUnitScaling = do
    -- list of variables
    let depVars   = getKeys kernelDefinition
        indepVars = getKeys $ _kodvLengths $ head $ _kdefPerDepVar kernelDefinition
    -- read observations
    observations <- filterVarsInObs depVars indepVars <$> readObservations inObsFile
    -- dependent variables
    let yPerDepVar = extractDepVars depVars observations
    hPutStrLn stderr "Calculating total variances"
    let variancesPerDepVar = calculateVariances depVars observations
        variancesPerDepVar2 = calculateVariances2 depVars observations
    -- read and prepare prediction grids
    hPutStrLn stderr "Preparing prediction grid"
    indepVarsPredGrid <- readIndepVarsPredGrid indepVars observations indepVarsPredGridSettings
    depVarsPredGrid   <- traverse (readDepVarsPredGrid depVars indepVars) depVarsPredGridSettings
    let searchPos = fmap (extractGridPos depVars) depVarsPredGrid
    let supplement = createCoreSupplement indepVarsPredGrid
    -- prepare permutations
    hPutStrLn stderr "Preparing permutations"
    let permutations = createPermutations kernelDefinition indepVarsPredGrid depVarsPredGrid
        permutations2 = createPermutations2 indepVarsPredGrid depVars kernelDefinition yPerDepVar variancesPerDepVar2 searchPos
        numPerms = length permutations
        numPerms2 = length permutations
    -- run analysis pipeline
    hPutStrLn stderr "Running analysis"
    case outMode of
        CoreOutObsWeight nrTopObs -> do
            Con.runConduitRes $
                ConC.yieldMany permutations
                .| ConC.conduitVector 100
                .| ConAA.asyncMapC numThreads (V.map (coreOutObsWeight spatDistUnitScaling nrTopObs supplement depVars observations))
                .| ConC.concat
                .| progress 1000 (Just numPerms)
                .| ConC.concatMap id
                .| sinkNamedCSV outFile
        CoreOutInterpolSamples nrRandomIts maybeSeed maybeSamplingRange -> do
            let range = case maybeSamplingRange of
                    Just OneSigma         -> (0.159, 0.841)
                    Just TwoSigma         -> (0.025, 0.975)
                    Just FullDistribution -> (0,1)
                    Nothing               -> (0,1)
            rng <- case maybeSeed of
                    Nothing   -> newIOGenM =<< R.getStdGen
                    Just seed -> newIOGenM $ mkStdGen seed
            randomIts <- forM permutations $ \p -> do
                 rss <- forM  [0..nrRandomIts-1] $ \i -> do
                    rs <- forM depVars $ \d -> do
                            r <- R.uniformRM range rng
                            return (d, r)
                    return (i, ValuesPerDepVar rs)
                 return (p, rss)
            Con.runConduitRes $
                ConC.yieldMany randomIts
                .| ConC.conduitVector 100
                .| ConAA.asyncMapC numThreads (V.map (coreOutInterpolSamples spatDistUnitScaling variancesPerDepVar supplement depVars observations))
                .| ConC.concat
                .| progress 1000 (Just numPerms)
                .| ConC.concatMap id
                .| sinkNamedCSV outFile
        --otherNormalMode -> do -- CoreOutShort or CoreOutFull
        --    Con.runConduitRes $
        --        ConC.yieldMany permutations
                -- non-chunked solution
                -- .| ConAA.asyncMapC numThreads (coreNormal spatDistUnitScaling otherNormalMode variancesPerDepVar supplement depVars observations)
        --        .| ConC.conduitVector 100
        --        .| ConAA.asyncMapC numThreads (V.map (coreNormal spatDistUnitScaling otherNormalMode variancesPerDepVar supplement depVars observations))
        --        .| ConC.concat
        --        .| progress 1000 (Just numPerms)
        --        .| normalise normalisation
        --        .| sinkNamedCSV outFile
        otherNormalMode -> do -- CoreOutShort or CoreOutFull
            Con.runConduitRes $
                ConC.yieldMany permutations2
                .| ConL.groupBy groupingCriteria1
                .| ConAA.asyncMapC numThreads (coreNormal2 spatDistUnitScaling supplement observations)
                .| ConL.groupBy groupingCriteria2
                .| ConC.map mymerge
                -- .| ConAA.asyncMapC numThreads (coreNormal spatDistUnitScaling otherNormalMode variancesPerDepVar supplement depVars observations)
                .| progress 1000 (Just numPerms)
                .| normalise normalisation
                .| sinkNamedCSV outFile
    hPutStrLn stderr "Done"
        where
            groupingCriteria1 :: CorePermutation2 -> CorePermutation2 -> Bool
            groupingCriteria1
                (CorePermutation2 _ tsi1 crossi1 depVar1 _ _ _ _)
                (CorePermutation2 _ tsi2 crossi2 depVar2 _ _ _ _) =
                     tsi1 == tsi2 && crossi1 == crossi2 && depVar1 == depVar2
            groupingCriteria2 :: [(CorePermutation, a)] -> [(CorePermutation, a)] -> Bool
            groupingCriteria2 l1 l2 =
                let (CorePermutation _ _ _ tsi1 crossi1 , _) = head l1
                    (CorePermutation _ _ _ tsi2 crossi2 , _) = head l2
                in tsi1 == tsi2 && crossi1 == crossi2

calculateVariances2 :: [DepVarName] -> V.Vector Observation -> [Double]
calculateVariances2 depVars obs =
    let valuesPerDepVar = map (\depVar -> VU.convert $ V.map (getValueOneBasicObsOneDepVar depVar) obs) depVars
    in map calculateVariance valuesPerDepVar
    where
        getValueOneBasicObsOneDepVar :: DepVarName -> Observation -> Double
        getValueOneBasicObsOneDepVar depVar (Observation _ _ (HyperPos _ depVarPos) _) = lookupUnsafe depVarPos depVar
        calculateVariance :: VU.Vector Double -> Double
        calculateVariance values =
            let nrSamples = fromIntegral $ VU.length values
            in varSample_ nrSamples values

calculateVariances :: [DepVarName] -> V.Vector Observation -> DepVarVariances
calculateVariances depVars obs =
    let valuesPerDepVar = map (\depVar -> (depVar, VU.convert $ V.map (getValueOneBasicObsOneDepVar depVar) obs)) depVars
    in ValuesPerDepVar $ map calculateVariance valuesPerDepVar
    where
        getValueOneBasicObsOneDepVar :: DepVarName -> Observation -> Double
        getValueOneBasicObsOneDepVar depVar (Observation _ _ (HyperPos _ depVarPos) _) = lookupUnsafe depVarPos depVar
        calculateVariance :: (DepVarName, VU.Vector Double) -> (DepVarName, Double)
        calculateVariance (depVar,values) =
            let nrSamples = fromIntegral $ VU.length values
            in (depVar, varSample_ nrSamples values)

readIndepVarsPredGrid :: [String] -> V.Vector Observation -> IndepVarsPredGridSettings -> IO IndepVarsPredGrid
-- spatiotemporal case
readIndepVarsPredGrid
    _
    observations
    (SpaceTimeGridSettings inSpatGridFile inTempGrid
        (CoreSupplementSettings distanceFilterThresholds inSpatDistFile inObsTempSamplesFile noOrderCheck)
    ) = do
    hPutStrLn stderr "Assuming a spatiotemporal system"
    -- read spatial grid
    inSpatGrid <- readSpatPos inSpatGridFile
    -- read spatial distances
    inSpatDists <- case inSpatDistFile of
        Nothing   -> pure Nothing
        Just path -> case takeExtension path of
            ".cbor" -> Just <$> readSpatDist (ReadSpatDistDeserialise path)
            _       -> Just <$> readSpatDist (ReadSpatDistParse noOrderCheck observations (Just inSpatGrid) path)
    -- read temporal distances
    inObsTempSamples <- case inObsTempSamplesFile of
        Nothing   -> pure Nothing
        Just path -> case takeExtension path of
            ".cbor" -> Just <$> readTempSamp (ReadTempSampDeserialise path)
            _       -> Just <$> readTempSamp (ReadTempSampParse noOrderCheck observations path)
    -- ordering of distance filter tresholds not necessary here
    -- return grid
    return $ SpaceTimeGrid inSpatGrid inTempGrid distanceFilterThresholds inSpatDists inObsTempSamples
-- arbitrary dimension case
readIndepVarsPredGrid
    indepVarsWanted
    _
    (ArbitraryDimGridSettings inArbitraryDimGridFile
        (CoreSupplementSettings distanceFilterThresholdsRaw _ _ _)
    ) = do
    hPutStrLn stderr "Assuming an arbitrary-dimension system"
    -- read arbitrary-dimension grid
    inArbitraryDimPosRaw <- readArbitraryDimPos inArbitraryDimGridFile
    let inArbitraryDimPos = filterVarsInArbitraryPos indepVarsWanted inArbitraryDimPosRaw
    -- filter distance filter tresholds
    let distanceFilterThresholds = fmap (filterDistanceThresholds indepVarsWanted) distanceFilterThresholdsRaw
    -- return grid
    return $ ArbitraryDimGrid inArbitraryDimPos distanceFilterThresholds

readDepVarsPredGrid :: [String] -> [String] -> DepVarsPredGridSettings -> IO DepVarsPredGrid
readDepVarsPredGrid depVars _ (DirectDepVarsGridSettings depVarsPos) = do
    -- reorder depVarsPos
    let depVarsPosReordered = map (filterByKey depVars) depVarsPos
    -- return grid
    return $ DepVarsPredGrid $ map DepVarsPredPosDirect depVarsPosReordered
readDepVarsPredGrid depVars indepVars (SearchObsDepVarsGridSettings path) = do
    -- read search observations
    obsVec <- readObservations path
    let obsVecReordered = filterVarsInObs depVars indepVars obsVec
        searchObservations = V.toList obsVecReordered
    -- return grid
    return $ DepVarsPredGrid $ map DepVarsPredPosSearchObs searchObservations

createCoreSupplement :: IndepVarsPredGrid -> CoreSupplement
createCoreSupplement (SpaceTimeGrid _ _ distFilterThresholds maybeSpatDistMap maybeTempSamples) =
    CoreSupplement distFilterThresholds maybeSpatDistMap maybeTempSamples
createCoreSupplement (ArbitraryDimGrid _ distFilterThresholds) =
    CoreSupplement distFilterThresholds Nothing Nothing

createPermutations2 :: IndepVarsPredGrid -> [DepVarName] -> KernelDefinition -> [M.Vector M.R] -> [Double] -> Maybe [M.Vector M.R] -> [CorePermutation2]
-- spatiotemporal, search
createPermutations2 (SpaceTimeGrid inSpatGrid inTempGrid _ _ inObsTempSamples) depVars (KernelDefinition kernelsPerDepVar) yPerDepVar variancePerDepVar (Just searchPosPerDepVar) = do
    tempSamp <- [0..(nrTempSamples inObsTempSamples - 1)]
    (depVar, kernel, y, var, search) <- zip5 depVars kernelsPerDepVar yPerDepVar variancePerDepVar searchPosPerDepVar
    absRelTempPos <- inTempGrid
    let tempPos = case absRelTempPos of
            AbsTempPos x -> x
            RelTempPos x -> x --case depPos of
                --(DepVarsPredPosSearchObs (Observation _ _ (HyperPos (IndepSpatTempPos (SpatTempPos _ (TempPos obsAge))) _) _)) -> obsAge + x
               -- _ -> throwL "--tempGrid relative(...) can only be used with --searchObsFile"
    spatPos <- V.toList inSpatGrid
    return $ CorePermutation2 (IndepSpatTempPos (SpatTempPos spatPos (TempPos tempPos))) tempSamp 0 depVar kernel y var (Just search)
-- spatiotemporal, no search
createPermutations2 (SpaceTimeGrid inSpatGrid inTempGrid _ _ inObsTempSamples) depVars (KernelDefinition kernelsPerDepVar) yPerDepVar variancePerDepVar Nothing = do
    tempSamp <- [0..(nrTempSamples inObsTempSamples - 1)]
    (depVar, kernel, y, var) <- zip4 depVars kernelsPerDepVar yPerDepVar variancePerDepVar
    absRelTempPos <- inTempGrid
    let tempPos = case absRelTempPos of
            AbsTempPos x -> x
            RelTempPos _ -> throwL "--tempGrid relative(...) can only be used with --searchObsFile"
    spatPos <- V.toList inSpatGrid
    return $ CorePermutation2 (IndepSpatTempPos (SpatTempPos spatPos (TempPos tempPos))) tempSamp 0 depVar kernel y var Nothing
-- arbitrary dims, search
createPermutations2 (ArbitraryDimGrid gridPos _) depVars (KernelDefinition kernelsPerDepVar) yPerDepVar variancePerDepVar (Just searchPosPerDepVar) = do
    (depVar, kernel, y, var, search) <- zip5 depVars kernelsPerDepVar yPerDepVar variancePerDepVar searchPosPerDepVar
    indepPos <- V.toList gridPos
    return $ CorePermutation2 (IndepArbitraryDimPos indepPos) 0 0 depVar kernel y var (Just search)
-- arbitrary dims, no search
createPermutations2 (ArbitraryDimGrid gridPos _) depVars (KernelDefinition kernelsPerDepVar) yPerDepVar variancePerDepVar Nothing = do
    (depVar, kernel, y, var) <- zip4 depVars kernelsPerDepVar yPerDepVar variancePerDepVar
    indepPos <- V.toList gridPos
    return $ CorePermutation2 (IndepArbitraryDimPos indepPos) 0 0 depVar kernel y var Nothing

createPermutations :: KernelDefinition -> IndepVarsPredGrid -> Maybe DepVarsPredGrid -> [CorePermutation]
-- spatiotemporal, search
createPermutations kernelDef (SpaceTimeGrid inSpatGrid inTempGrid _ _ inObsTempSamples) (Just (DepVarsPredGrid depVarPos)) = do
    tempSamp <- [0..(nrTempSamples inObsTempSamples - 1)]
    absRelTempPos <- inTempGrid
    depPos <- depVarPos
    let tempPos = case absRelTempPos of
            AbsTempPos x -> x
            RelTempPos x -> case depPos of
                (DepVarsPredPosSearchObs (Observation _ _ (HyperPos (IndepSpatTempPos (SpatTempPos _ (TempPos obsAge))) _) _)) -> obsAge + x
                _ -> throwL "--tempGrid relative(...) can only be used with --searchObsFile"
    spatPos <- V.toList inSpatGrid
    return $ CorePermutation (IndepSpatTempPos (SpatTempPos spatPos (TempPos tempPos))) (Just depPos) kernelDef tempSamp 0
-- spatiotemporal, no search
createPermutations kernelDef (SpaceTimeGrid inSpatGrid inTempGrid _ _ inObsTempSamples) Nothing = do
    tempSamp <- [0..(nrTempSamples inObsTempSamples - 1)]
    absRelTempPos <- inTempGrid
    let tempPos = case absRelTempPos of
            AbsTempPos x -> x
            RelTempPos _ -> throwL "--tempGrid relative(...) can only be used with --searchObsFile"
    spatPos <- V.toList inSpatGrid
    return $ CorePermutation (IndepSpatTempPos (SpatTempPos spatPos (TempPos tempPos))) Nothing kernelDef tempSamp 0
-- arbitrary dims, search
createPermutations kernelDef (ArbitraryDimGrid gridPos _) (Just (DepVarsPredGrid depVarPos)) =
    [ CorePermutation (IndepArbitraryDimPos indepPos) (Just depPos) kernelDef 0 0
    | indepPos <- V.toList gridPos, depPos <- depVarPos]
-- arbitrary dims, no search
createPermutations kernelDef (ArbitraryDimGrid gridPos _) Nothing =
    [ CorePermutation (IndepArbitraryDimPos indepPos) Nothing kernelDef 0 0
    | indepPos <- V.toList gridPos]

nrTempSamples :: Maybe TempSampleMatrix -> Int
nrTempSamples Nothing                         = 1
nrTempSamples (Just (TempSampleMatrix n _ _)) = n

normalise :: Monad m => Normalisation -> Con.ConduitT SearchResult SearchResult m ()
normalise NoNorm = ConC.map id
normalise NormBySpace =
       ConL.groupBy groupingCriteria
    .| ConC.map scaleProbs
    .| ConC.concat
    where
    groupingCriteria :: SearchResult -> SearchResult -> Bool
    groupingCriteria
        (SearchResult (CorePermutation (IndepSpatTempPos (SpatTempPos _ t1)) dv1 alg1 tri1 _) _ _)
        (SearchResult (CorePermutation (IndepSpatTempPos (SpatTempPos _ t2)) dv2 alg2 tri2 _) _ _) =
            t1 == t2 && dv1 == dv2 && alg1 == alg2 && tri1 == tri2
    groupingCriteria _ _ = False
    scaleProbs :: [SearchResult] -> [SearchResult]
    scaleProbs stps =
        let maybeLogLikelihoods = map getLogL stps
            probabilities = case catMaybes maybeLogLikelihoods of
                []          -> repeat Nothing
                logls ->
                    -- https://stats.stackexchange.com/questions/66616/converting-normalizing-very-small-likelihood-values-to-probability
                    -- no explicit underflow handling implemented, because
                    -- I think Haskell sets the output of exp reliably to zero
                    -- for underflowing doubles
                    let maxlogl = maximum logls
                        ls = map (\logl -> exp $ logl - maxlogl) logls
                        sumls = foldSum ls
                    in map (\l -> Just $ l / sumls) ls
        in zipWith setLogL stps probabilities
    getLogL :: SearchResult -> Maybe Double
    getLogL (SearchResult _ _ (Just (SearchLikelihood _ logL _))) = Just logL
    getLogL _                                                     = Nothing
    setLogL :: SearchResult -> Maybe Double -> SearchResult
    setLogL stp@(SearchResult _ _ (Just slh@(SearchLikelihood {}))) p =
        stp { _srLikelihood = Just slh { _slhProbability = p } }
    setLogL stp _ = stp

{-# LANGUAGE ScopedTypeVariables #-}

module LocEst.CLI.Search where

import           LocEst.CLI.Utils
import           LocEst.CoreAlgorithms
import           LocEst.Exceptions
import           LocEst.MathUtils
import           LocEst.Parsers
import           LocEst.Types

import qualified Control.Monad                 as OP
import           Data.Conduit                  ((.|))
import qualified Data.Conduit                  as Con
import qualified Data.Conduit.Algorithms.Async as ConAA
import qualified Data.Conduit.Combinators      as ConC
import qualified Data.Conduit.List             as ConL
import           Data.Maybe                    (catMaybes)
import qualified Data.Vector                   as V
import           System.FilePath               (takeExtension)
import           System.IO                     (hPutStrLn, stderr)
import System.Random.Stateful as R

data SearchOptions = SearchOptions
    { _searchInObservationFile  :: FilePath
    , _searchSearchGridSettings :: SearchGridSettings
    , _searchAlgorithm          :: KernelDefinition
    , _normalize                :: Normalization
    , _searchOutFile            :: FilePath
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
    , _stgsSupplementSettings :: SpaceTimeCoreSupplementSettings
} | ArbitraryDimGridSettings {
      _adgsInArbitraryDimGridFile :: FilePath
}

data SpaceTimeCoreSupplementSettings = SpaceTimeCoreSupplementSettings {
      _stcsSpaceTimeMinFilter   :: (Double,Double)
    , _stcsSpaceTimeMaxFilter   :: (Double,Double)
    , _stcsInSpatDistFile       :: Maybe FilePath
    , _stcsInObsTempSamplesFile :: Maybe FilePath
    , _stcsNoOrderCheck         :: Bool
}

runSearch :: SearchOptions -> Int -> IO ()
runSearch (
    SearchOptions
        inObsFile
        (SearchGridSettings indepVarsPredGridSettings depVarsPredGridSettings)
        kernelDefinition
        normalization
        outFile
        outMode
    ) numThreads = do
    -- read observations
    observations <- readObservations inObsFile
    -- read and prepare prediction grids
    hPutStrLn stderr "Preparing prediction grid"
    indepVarsPredGrid <- readIndepVarsPredGrid kernelDefinition observations indepVarsPredGridSettings
    depVarsPredGrid   <- traverse (readDepVarsPredGrid kernelDefinition observations) depVarsPredGridSettings
    let supplement = createCoreSupplement indepVarsPredGrid
    -- prepare permutations
    hPutStrLn stderr "Preparing permutations"
    let permutations = createPermutations kernelDefinition indepVarsPredGrid depVarsPredGrid
        numPerms = length permutations
    -- run analysis pipeline
    hPutStrLn stderr "All preparations ready"
    hPutStrLn stderr "Running analysis"
    case outMode of
        CoreOutObsWeight nrTopObs -> do
            Con.runConduitRes $
                ConC.yieldMany permutations
                .| ConAA.asyncMapC numThreads (coreOutObsWeight nrTopObs supplement observations)
                .| progress 1000 (Just numPerms)
                .| ConC.concatMap id
                .| sinkNamedCSV outFile
        CoreOutInterpolSamples nrRandomIts maybeSeed maybeSamplingRange -> do
            let depVarsFromAlg = getKeys kernelDefinition
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
                    rs <- forM depVarsFromAlg $ \d -> do
                            -- r <- R.uniformDouble01M rng
                            r <- R.uniformRM range rng
                            return (d, r)
                    return (i, ValuesPerDepVar rs)
                 return (p, rss)
            Con.runConduitRes $
                ConC.yieldMany randomIts
                .| ConAA.asyncMapC numThreads (coreOutInterpolSamples supplement observations)
                .| progress 1000 (Just numPerms)
                .| ConC.concatMap id
                .| sinkNamedCSV outFile
        CoreOutShort -> do
            Con.runConduitRes $
                ConC.yieldMany permutations
                .| ConAA.asyncMapC numThreads (coreNormal CoreOutShort supplement observations)
                .| progress 1000 (Just numPerms)
                .| normalize normalization
                .| sinkNamedCSV outFile
        CoreOutFull -> do
            Con.runConduitRes $
                ConC.yieldMany permutations
                .| ConAA.asyncMapC numThreads (coreNormal CoreOutFull supplement observations)
                -- probably faster:
                -- .| ConC.conduitVector 1000
                -- .| ConAA.asyncMapC numThreads (V.map (coreNormal CoreOutFull supplement observations))
                -- .| ConC.concat
                .| progress 1000 (Just numPerms)
                .| normalize normalization
                .| sinkNamedCSV outFile
    hPutStrLn stderr "Done"

readIndepVarsPredGrid ::
       KernelDefinition
    -> V.Vector Observation
    -> IndepVarsPredGridSettings
    -> IO IndepVarsPredGrid
-- spatiotemporal case
readIndepVarsPredGrid
    (KernelDefinition kernelsPerDepVars)
    observations
    (SpaceTimeGridSettings
        inSpatGridFile
        inTempGrid
        (SpaceTimeCoreSupplementSettings
            inSpaceTimeMinFilter
            inSpaceTimeMaxFilter
            inSpatDistFile
            inObsTempSamplesFile
            noOrderCheck
        )
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
    -- validation obsFile
    case (_hyposIndepVarsPos . _obsPos) $ V.head observations of
            IndepSpatTempPos _     -> return ()
            IndepArbitraryDimPos _ ->
                throwLIO "spatiotemporal positions in --obsFile not readable"
    -- validation kernelDefinition
    let indepVarsFromAlg = map (getKeys . _kodvLengths) kernelsPerDepVars
    OP.unless (head indepVarsFromAlg == ["space", "time"]) $
        throwLIO "independent variable names in --kerndef not equal to \"space\" and \"time\""
    OP.unless (allEqual indepVarsFromAlg) $
        throwLIO "independent variable names in --kerndef not all equal across kernel \
                 \definitions for all dependent variables"
    -- return grid
    return $ SpaceTimeGrid inSpatGrid inTempGrid inSpaceTimeMinFilter inSpaceTimeMaxFilter inSpatDists inObsTempSamples
-- arbitrary dimension case
readIndepVarsPredGrid
    (KernelDefinition kernelsPerDepVars)
    observations
    (ArbitraryDimGridSettings inArbitraryDimGridFile) = do
    hPutStrLn stderr "Assuming an arbitrary-dimension system"
    -- read arbitrary-dimension grid
    inArbitraryDimPos <- readArbitraryDimPos inArbitraryDimGridFile
    -- validation obsFile
    let indepVarsFromObs = case (_hyposIndepVarsPos . _obsPos) $ V.head observations of
            IndepSpatTempPos _     -> []
            IndepArbitraryDimPos x -> getKeys x
    let indepVarsFromGrid = getKeys $ V.head inArbitraryDimPos
    OP.when (indepVarsFromObs /= indepVarsFromGrid) $ do
        throwLIO "independent variable names in --obsFile and --anyGridFile not equal"
    -- validation kernelDefinition
    let indepVarsFromAlg = map (getKeys . _kodvLengths) kernelsPerDepVars
    OP.unless (head indepVarsFromAlg == indepVarsFromGrid) $
        throwLIO "independent variable names in --kerndef and --anyGridFile not equal"
    OP.unless (allEqual indepVarsFromAlg) $
        throwLIO "independent variable names in --kerndef not all equal across kernel \
                 \definitions for all dependent variables"
    -- return grid
    return $ ArbitraryDimGrid inArbitraryDimPos

readDepVarsPredGrid ::
       KernelDefinition
    -> V.Vector Observation
    -> DepVarsPredGridSettings
    -> IO DepVarsPredGrid
readDepVarsPredGrid
    kernelDefinition
    observations
    (DirectDepVarsGridSettings depVarsPos) = do
    -- get search positions
    let depVarsFromGrid = getKeys $ head depVarsPos
    -- validation obsFile
    let depVarsFromObs = getKeys $ (_hyposDepVarsPos . _obsPos) $ V.head observations
    OP.when (depVarsFromObs /= depVarsFromGrid) $ do
        throwLIO "dependent variable names in --obsFile and --searchDepVarsPos not equal"
    -- validation kernelDefinition
    let depVarsFromAlg = getKeys kernelDefinition
    OP.unless (depVarsFromAlg == depVarsFromGrid) $
        throwLIO "dependent variable names in --kerndef and --searchDepVarsPos not equal"
    -- return grid
    return $ DepVarsPredGrid $ map DepVarsPredPosDirect depVarsPos
readDepVarsPredGrid
    kernelDefinition
    observations
    (SearchObsDepVarsGridSettings path) = do
    -- read search observations
    searchObservations <- V.toList <$> readObservations path
    let depVarsFromGrid = getKeys $ (_hyposDepVarsPos . _obsPos) $ head searchObservations
    -- validation obsFile
    let depVarsFromObs = getKeys $ (_hyposDepVarsPos . _obsPos) $ V.head observations
    OP.when (depVarsFromObs /= depVarsFromGrid) $ do
        throwLIO "dependent variable names in --obsFile and --searchObsFile not equal"
    -- validation kernelDefinition
    let depVarsFromAlg = getKeys kernelDefinition
    OP.unless (depVarsFromAlg == depVarsFromGrid) $
        throwLIO  "dependent variable names in --kerndef and --searchObsFile not equal"
    -- return grid
    return $ DepVarsPredGrid $ map DepVarsPredPosSearchObs searchObservations

createCoreSupplement :: IndepVarsPredGrid -> CoreSupplement
createCoreSupplement (SpaceTimeGrid _ _ spaceTimeMinFilter spaceTimeMaxFilter maybeSpatDistMap maybeTempSamples) =
    CoreSupplement spaceTimeMinFilter spaceTimeMaxFilter maybeSpatDistMap maybeTempSamples
createCoreSupplement (ArbitraryDimGrid _) =
    CoreSupplement (0,0) (infinity, infinity) Nothing Nothing

createPermutations :: KernelDefinition -> IndepVarsPredGrid -> Maybe DepVarsPredGrid -> [CorePermutation]
-- spatiotemporal, search
createPermutations kernelDef (SpaceTimeGrid inSpatGrid inTempGrid _ _ _ inObsTempSamples) (Just (DepVarsPredGrid depVarPos)) = do
    tempSamp <- [0..(nrTempSamples inObsTempSamples - 1)]
    absRelTempPos <- inTempGrid
    depPos <- depVarPos
    let tempPos = case absRelTempPos of
            AbsTempPos x -> x
            RelTempPos x -> case depPos of
                    (DepVarsPredPosSearchObs (Observation _ _ (HyperPos (IndepSpatTempPos (SpatTempPos _ (TempPos obsAge))) _))) -> obsAge + x
                    _ -> throwL ""
    spatPos <- V.toList inSpatGrid
    return $ CorePermutation (IndepSpatTempPos (SpatTempPos spatPos (TempPos tempPos))) (Just depPos) kernelDef tempSamp 0
-- spatiotemporal, no search
createPermutations kernelDef (SpaceTimeGrid inSpatGrid inTempGrid _ _ _ inObsTempSamples) Nothing = do
    tempSamp <- [0..(nrTempSamples inObsTempSamples - 1)]
    absRelTempPos <- inTempGrid
    let tempPos = case absRelTempPos of
            AbsTempPos x -> x
            RelTempPos _ -> throwL ""
    spatPos  <- V.toList inSpatGrid
    return $ CorePermutation (IndepSpatTempPos (SpatTempPos spatPos (TempPos tempPos))) Nothing kernelDef tempSamp 0
-- arbitrary dims, search
createPermutations kernelDef (ArbitraryDimGrid gridPos) (Just (DepVarsPredGrid depVarPos)) =
    [ CorePermutation (IndepArbitraryDimPos indepPos) (Just depPos) kernelDef 0 0
    | indepPos <- V.toList gridPos, depPos <- depVarPos]
-- arbitrary dims, no search
createPermutations kernelDef (ArbitraryDimGrid gridPos) Nothing =
    [ CorePermutation (IndepArbitraryDimPos indepPos) Nothing kernelDef 0 0
    | indepPos <- V.toList gridPos]

nrTempSamples :: Maybe TempSampleMatrix -> Int
nrTempSamples Nothing                         = 1
nrTempSamples (Just (TempSampleMatrix n _ _)) = n

normalize :: Monad m => Normalization -> Con.ConduitT SearchResult SearchResult m ()
normalize NoNorm = ConC.map id
normalize NormBySpace =
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

{-# LANGUAGE ScopedTypeVariables #-}

module LocEst.CLI.Search where

import           LocEst.CLI.Utils
import           LocEst.CoreAlgorithms
import           LocEst.Parsers
import           LocEst.Types
import           LocEst.Exceptions

import           Conduit                       (ResourceT)
import qualified Control.Monad                 as OP
import           Data.Conduit                  ((.|))
import qualified Data.Conduit                  as Con
import qualified Data.Conduit.Algorithms.Async as ConAA
import qualified Data.Conduit.Combinators      as ConC
import qualified Data.Conduit.List             as ConL
import           Data.Maybe                    (catMaybes)
import qualified Data.Vector                   as V
import           LocEst.MathUtils
import           System.FilePath               (takeExtension)
import           System.IO                     (hPutStrLn, stderr)

data SearchOptions = SearchOptions
    { _searchInObservationFile  :: FilePath
    , _searchSearchGridSettings :: SearchGridSettings
    , _searchAlgorithm          :: KernelDefinition
    , _normalize                :: Normalization
    , _numThreads               :: NumberOfThreads
    , _searchOutMode            :: CoreOutMode
    , _searchOutFile            :: FilePath
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
    , _stgsInTempGrid         :: [Int]
    , _stgsSupplementSettings :: SpaceTimeCoreSupplementSettings
} | ArbitraryDimGridSettings {
      _adgsInArbitraryDimGridFile :: FilePath
}

data SpaceTimeCoreSupplementSettings = SpaceTimeCoreSupplementSettings {
      _stcsSpaceTimeFilter      :: Maybe (Double,Double)
    , _stcsInSpatDistFile       :: Maybe FilePath
    , _stcsInObsTempSamplesFile :: Maybe FilePath
    , _stcsNoOrderCheck         :: Bool
}

runSearch :: SearchOptions -> IO ()
runSearch (
    SearchOptions
        inObsFile
        (SearchGridSettings indepVarsPredGridSettings depVarsPredGridSettings)
        algorithm
        normalization
        threads
        outMode
        outFile
    ) = do
    -- number of threads
    numThreads <- setNumberOfThreads threads
    -- read observations
    observations <- readObservations inObsFile
    -- read and prepare prediction grids
    hPutStrLn stderr "Preparing prediction grid"
    indepVarsPredGrid <- readIndepVarsPredGrid indepVarsPredGridSettings observations
    validateAlgorithmInterpol algorithm indepVarsPredGrid
    depVarsPredGrid   <- case depVarsPredGridSettings of
        Just x -> do
            res <- readDepVarsPredGrid x observations
            validateAlgorithmSearch algorithm res
            return $ Just res
        Nothing -> pure Nothing
    let searchGrid = SearchGrid indepVarsPredGrid depVarsPredGrid
        supplement = createCoreSupplement searchGrid
    -- prepare permutations
    hPutStrLn stderr "Preparing permutations"
    let permutations = createPermutations algorithm indepVarsPredGrid depVarsPredGrid
        numPerms = length permutations
    -- run analysis pipeline
    hPutStrLn stderr "All preparations ready"
    hPutStrLn stderr "Running analysis"
    Con.runConduitRes $
        ConC.yieldMany permutations
        -- main search algorithm
        -- 1. sequential
        -- .| ConL.map core
        -- 2. normal parallel
        .| ConAA.asyncMapC numThreads (core outMode supplement observations)
        -- 3. chunked parallel
        -- .| Con.conduitVector 100 .| ConAA.asyncMapC 5 (V.map core) .| ConL.concat
        -- print progress information
        .| progress 1000 (Just numPerms)
        -- split stream for different output cases
        .| processBasedOnSetting outMode outFile normalization
    hPutStrLn stderr "Done"

processBasedOnSetting :: CoreOutMode -> FilePath -> Normalization -> Con.ConduitT CoreOut Con.Void (ResourceT IO) ()
processBasedOnSetting CoreOutShort outFile normalization =
       mapOnlySearchResult
    .| normalize normalization
    .| sinkNamedCSV outFile
processBasedOnSetting CoreOutFull outFile normalization =
       mapOnlySearchResult
    .| normalize normalization
    .| sinkNamedCSV outFile
processBasedOnSetting (CoreOutObsWeight _) outFile _ =
       mapOnlyObsWeights
    .| ConC.concatMap id
    .| sinkNamedCSV outFile

mapOnlyObsWeights :: Con.ConduitT CoreOut (V.Vector ObsWeight) (ResourceT IO) ()
mapOnlyObsWeights = ConC.concatMap coreOutToObsWeights -- this translates to a mapMaybe
mapOnlySearchResult :: Con.ConduitT CoreOut SearchResult (ResourceT IO) ()
mapOnlySearchResult = ConC.concatMap coreOutToSearchResult
coreOutToObsWeights :: CoreOut -> Maybe (V.Vector ObsWeight)
coreOutToObsWeights (CoreObsWeight x) = Just x
coreOutToObsWeights _                 = Nothing
coreOutToSearchResult :: CoreOut -> Maybe SearchResult
coreOutToSearchResult (CoreSearchResult x) = Just x
coreOutToSearchResult _                    = Nothing

readIndepVarsPredGrid ::
       IndepVarsPredGridSettings
    -> V.Vector Observation
    -> IO IndepVarsPredGrid
readIndepVarsPredGrid
    (SpaceTimeGridSettings
        inSpatGridFile
        inTempGrid
        (SpaceTimeCoreSupplementSettings
            inSpaceTimeFilter
            inSpatDistFile
            inObsTempSamplesFile
            noOrderCheck
        )
    )
    observations = do
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
    -- input validation
    case (_hyposIndepVarsPos . _obsPos) $ V.head observations of
            IndepSpatTempPos _     -> return ()
            IndepArbitraryDimPos _ ->
                throwLIO "spatiotemporal positions in --obsFile not readable, maybe wrong column names"
    -- complete spatiotemporal grid
    return $ SpaceTimeGrid inSpatGrid inTempGrid inSpaceTimeFilter inSpatDists inObsTempSamples
readIndepVarsPredGrid
    (ArbitraryDimGridSettings
        inArbitraryDimGridFile
    )
    observations = do
    hPutStrLn stderr "Assuming an arbitrary-dimension system"
    -- read arbitrary-dimension grid
    inArbitraryDimPos <- readArbitraryDimPos inArbitraryDimGridFile
    -- input validation
    let varsFromObs = case (_hyposIndepVarsPos . _obsPos) $ V.head observations of
            IndepSpatTempPos _     -> []
            IndepArbitraryDimPos x -> getKeys x
    let varsFromGrid = getKeys $ V.head inArbitraryDimPos
    OP.when (varsFromObs /= varsFromGrid) $ do
        throwLIO "indep vars in --obsFile and --anyGridFile not equal"
    return $ ArbitraryDimGrid inArbitraryDimPos

validateAlgorithmInterpol :: KernelDefinition -> IndepVarsPredGrid -> IO ()
validateAlgorithmInterpol
    (KernelDefinition kernelsPerDepVars)
    (SpaceTimeGrid {}) = do
        let allIndepVarsFromAlg = map (getKeys . _kodvLengths) kernelsPerDepVars
            indepVarsFromGrid = ["space", "time"]
        OP.unless (allEqual allIndepVarsFromAlg) $
            throwLIO "indep var names not equal across kernel definitions"
        OP.unless (head allIndepVarsFromAlg == indepVarsFromGrid) $
            throwLIO "indep vars not equal to \"space\" and \"time\""
validateAlgorithmInterpol
    (KernelDefinition kernelsPerDepVars)
    (ArbitraryDimGrid arbitraryDimPos) = do
        let allIndepVarsFromAlg = map (getKeys . _kodvLengths) kernelsPerDepVars
            indepVarsFromGrid = getKeys $ V.head  arbitraryDimPos
        OP.unless (allEqual allIndepVarsFromAlg) $
            throwLIO "indep var names not equal across kernel definitions"
        OP.unless (head allIndepVarsFromAlg == indepVarsFromGrid) $
            throwLIO "indep vars in --anyGridFile and --algorithm not equal"

readDepVarsPredGrid ::
       DepVarsPredGridSettings
    -> V.Vector Observation
    -> IO DepVarsPredGrid
readDepVarsPredGrid
    searchObs
    observations = do
    let varsFromObs = getKeys $ (_hyposDepVarsPos . _obsPos) $ V.head observations
    case searchObs of
        DirectDepVarsGridSettings depVarsPos -> do
            let varsFromGrid = getKeys $ head depVarsPos
            OP.when (varsFromObs /= varsFromGrid) $ do
                throwLIO "dep vars in --obsFile and --depVars not equal"
            return $ DepVarsPredGrid $ map DepVarsPredPosDirect $ depVarsPos
        SearchObsDepVarsGridSettings path -> do
            searchObservations <- V.toList <$> readObservations path
            let varsFromSearchObs = getKeys $ (_hyposDepVarsPos . _obsPos) $ head searchObservations
            OP.when (varsFromObs /= varsFromSearchObs) $ do
                throwLIO "dep vars in --obsFile and --searchObsFile not equal"
            return $ DepVarsPredGrid $ map DepVarsPredPosSearchObs searchObservations

validateAlgorithmSearch :: KernelDefinition -> DepVarsPredGrid -> IO ()
validateAlgorithmSearch kernelDef (DepVarsPredGrid (DepVarsPredPosDirect x:_)) = do
        let depVarsFromAlg = getKeys kernelDef
            depVarsFromGrid = getKeys x
        OP.unless (depVarsFromAlg == depVarsFromGrid) $
            throwLIO "dep vars in --depVars and --algorithm not equal"
validateAlgorithmSearch kernelDef (DepVarsPredGrid (DepVarsPredPosSearchObs x:_)) = do
        let depVarsFromAlg = getKeys kernelDef
            depVarsFromGrid = getKeys $ (_hyposDepVarsPos . _obsPos) x
        OP.unless (depVarsFromAlg == depVarsFromGrid) $
            throwLIO "dep vars in --searchObsFile and --algorithm not equal"
validateAlgorithmSearch _ (DepVarsPredGrid _) = do
        throwLIO "dep vars empty"

createCoreSupplement :: SearchGrid -> CoreSupplement
createCoreSupplement (SearchGrid (SpaceTimeGrid _ _ spaceTimeFilter maybeSpatDistMap maybeTempSamples) _) =
    CoreSupplement spaceTimeFilter maybeSpatDistMap maybeTempSamples
createCoreSupplement (SearchGrid (ArbitraryDimGrid _) _) =
    CoreSupplement Nothing Nothing Nothing

createPermutations :: KernelDefinition -> IndepVarsPredGrid -> Maybe DepVarsPredGrid -> [CorePermutation]
createPermutations kernelDef (SpaceTimeGrid inSpatGrid inTempGrid _ _ inObsTempSamples) (Just (DepVarsPredGrid depVarPos)) =
    [ CorePermutation (IndepSpatTempPos (SpatTempPos spatPos (TempPos tempPos))) (Just depPos) kernelDef tempSamp
    | tempSamp <- [0..(nrTempSamples inObsTempSamples - 1)], depPos <- depVarPos, tempPos <- inTempGrid, spatPos <- V.toList inSpatGrid]
createPermutations kernelDef (SpaceTimeGrid inSpatGrid inTempGrid _ _ inObsTempSamples) Nothing =
    [ CorePermutation (IndepSpatTempPos (SpatTempPos spatPos (TempPos tempPos))) Nothing kernelDef tempSamp
    | tempSamp <- [0..(nrTempSamples inObsTempSamples - 1)], tempPos <- inTempGrid, spatPos  <- V.toList inSpatGrid]
createPermutations kernelDef (ArbitraryDimGrid gridPos) (Just (DepVarsPredGrid depVarPos)) =
    [ CorePermutation (IndepArbitraryDimPos indepPos) (Just depPos) kernelDef 0
    | indepPos <- V.toList gridPos, depPos <- depVarPos]
createPermutations kernelDef (ArbitraryDimGrid gridPos) Nothing =
    [ CorePermutation (IndepArbitraryDimPos indepPos) Nothing kernelDef 0
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
        (SearchResult (CorePermutation (IndepSpatTempPos (SpatTempPos _ t1)) dv1 alg1 tri1) _ _)
        (SearchResult (CorePermutation (IndepSpatTempPos (SpatTempPos _ t2)) dv2 alg2 tri2) _ _) =
            t1 == t2 && dv1 == dv2 && alg1 == alg2 && tri1 == tri2
    groupingCriteria _ _ = False
    scaleProbs :: [SearchResult] -> [SearchResult]
    scaleProbs stps =
        let probs = map getProb stps
            rescaledProbs = case catMaybes probs of
                [] -> repeat Nothing
                xs -> map (\x -> Just $ x / foldSum xs) xs
        in zipWith setProb stps rescaledProbs
    getProb :: SearchResult -> Maybe Double
    getProb stp@(SearchResult {}) = _srProbability stp
    setProb :: SearchResult -> Maybe Double -> SearchResult
    setProb stp@(SearchResult {}) p = stp {_srProbability = p}

allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual (x:xs) = all (== x) xs

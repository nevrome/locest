{-# LANGUAGE ScopedTypeVariables #-}

module LocEst.CLI.Search where

import           LocEst.CLI.Utils
import           LocEst.CoreAlgorithms
import           LocEst.Exceptions
import           LocEst.MathUtils
import           LocEst.Parsers
import           LocEst.Types

import           Conduit                       (ResourceT)
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
        kernelDefinition
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
            inSpaceTimeFilter
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
    return $ SpaceTimeGrid inSpatGrid inTempGrid inSpaceTimeFilter inSpatDists inObsTempSamples
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
createCoreSupplement (SpaceTimeGrid _ _ spaceTimeFilter maybeSpatDistMap maybeTempSamples) =
    CoreSupplement spaceTimeFilter maybeSpatDistMap maybeTempSamples
createCoreSupplement (ArbitraryDimGrid _) =
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

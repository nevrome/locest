{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocEst.CLI.Search where

import           LocEst.CLI.Utils
import           LocEst.CoreAlgorithms
import           LocEst.Parsers
import           LocEst.Types
import           LocEst.Utils

import           Conduit                       (MonadIO, liftIO, ResourceT)
import           Control.Exception             (throw)
import qualified Control.Monad                 as OP
import qualified Control.Monad.Except          as E
import           Data.Conduit                  ((.|))
import qualified Data.Conduit                  as Con
import qualified Data.Conduit.Algorithms.Async as ConAA
import qualified Data.Conduit.Combinators      as ConC
import qualified Data.Conduit.List             as ConL
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                    (catMaybes)
import           LocEst.MathUtils
import           System.IO                     (hPutStrLn, stderr)
import qualified Data.Vector as V
import System.FilePath (takeExtension)

data SearchOptions = SearchOptions
    { _searchInObservationFile  :: FilePath
    , _searchSearchGridSettings :: SearchGridSettings
    , _searchAlgorithm          :: KernelDefinition
    , _normalize                :: Normalization
    , _numThreads               :: NumberOfThreads
    , _searchOutMode            :: SearchOutMode
    }

data SearchGridSettings = SearchGridSettings {
      _searchPosSetIndepVarsGrid :: IndepVarsPredGridSettings
    , _searchPosSetDepVarsGrid   :: Maybe [DepVarsPos]
}

data IndepVarsPredGridSettings = SpaceTimeGridSettings {
      _stgsInSpatGridFile       :: FilePath
    , _stgsInTempGrid           :: [Int]
    , _stgsSpaceTimeFilter      :: Maybe (Double,Double)
    , _stgsInSpatDistFile       :: Maybe FilePath
    , _stgsInObsTempSamplesFile :: Maybe FilePath
    , _stgsNoOrderCheck         :: Bool
} | ArbitraryDimGridSettings {
      _adgsInArbitraryDimGridFile :: FilePath
}

data SearchOutMode =
      SearchOutShort FilePath
    | SearchOutFull FilePath
    | SearchOutObsWeight Int FilePath

determineCoreOutMode :: SearchOutMode -> (CoreOutMode,FilePath)
determineCoreOutMode (SearchOutShort p)     = (CoreOutShort,p)
determineCoreOutMode (SearchOutFull p)      = (CoreOutFull,p)
determineCoreOutMode (SearchOutObsWeight n p) = (CoreOutObsWeight n,p)

runSearch :: SearchOptions -> IO ()
runSearch (
    SearchOptions
        inObsFile
        (SearchGridSettings indepVarsPredGridSettings depVarsPredGridSettings)
        algorithm
        normalization
        threads
        outMode
    ) = do
    -- number of threads
    numThreads <- setNumberOfThreads threads
    -- outmode
    let (coreOutMode,outFile) = determineCoreOutMode outMode
    -- read observations
    observations <- readObservations inObsFile
    -- read and prepare prediction grids
    hPutStrLn stderr "Preparing prediction grid"
    indepVarsPredGrid <- readIndepVarsPredGrid indepVarsPredGridSettings observations
    depVarsPredGrid   <- case depVarsPredGridSettings of
        Just x  -> do
            res <- readDepVarsPredGrid x observations
            validateAlgorithmSearch algorithm res
            return $ Just res
        Nothing -> pure Nothing
    validateAlgorithmInterpol algorithm indepVarsPredGrid
    let searchGrid = SearchGrid indepVarsPredGrid depVarsPredGrid
        supplement = createCoreSupplement searchGrid
    -- validate algorithm settings
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
        .| ConAA.asyncMapC numThreads (\x -> E.runExcept (core coreOutMode supplement observations x))
        -- 3. chunked parallel
        -- .| Con.conduitVector 100 .| ConAA.asyncMapC 5 (V.map core) .| ConL.concat
        -- print progress information
        .| progress 1000 (Just numPerms)
        -- split stream to report the error cases and write the good results to the file system
        .| Con.getZipSink (
                -- errors
                Con.ZipSink ( mapOnlyLefts .| ConL.groupOn id .| ConC.mapM_ printErrors ) *>
                -- results
                Con.ZipSink ( mapOnlyRights .| processBasedOnSetting outMode normalization )
            )
    hPutStrLn stderr "Done"

processBasedOnSetting :: SearchOutMode -> Normalization -> Con.ConduitT CoreOut Con.Void (ResourceT IO) ()
processBasedOnSetting (SearchOutShort outFile) normalization =
       mapOnlySearchResult
    .| normalize normalization
    .| sinkNamedCSV outFile
processBasedOnSetting (SearchOutFull outFile) normalization =
       mapOnlySearchResult
    .| normalize normalization
    .| sinkNamedCSV outFile
processBasedOnSetting (SearchOutObsWeight _ outFile) normalization =
       mapOnlyObsWeights
    .| ConC.concatMap id
    .| sinkNamedCSV outFile

mapOnlyObsWeights = ConC.concatMap coreOutToObsWeights
mapOnlySearchResult = ConC.concatMap coreOutToSearchResult
coreOutToObsWeights :: CoreOut -> Maybe (V.Vector ObsWeight)
coreOutToObsWeights (CoreObsWeight x) = Just x
coreOutToObsWeights _                 = Nothing
coreOutToSearchResult :: CoreOut -> Maybe SearchResult
coreOutToSearchResult (CoreSearchResult x) = Just x
coreOutToSearchResult _                    = Nothing

mapOnlyLefts = ConC.concatMap leftToJust -- this translates to a mapMaybe
mapOnlyRights = ConC.concatMap rightToJust
rightToJust :: Either a b -> Maybe b
rightToJust (Right x) = Just x
rightToJust _         = Nothing
leftToJust :: Either a b -> Maybe a
leftToJust (Left x) = Just x
leftToJust _        = Nothing

printErrors :: MonadIO m => NE.NonEmpty LOCESTException -> m ()
printErrors errMsg = liftIO $ hPutStrLn stderr (show (length errMsg) ++ " * " ++ renderLOCESTException (NE.head errMsg))

readIndepVarsPredGrid ::
       IndepVarsPredGridSettings
    -> V.Vector Observation
    -> IO IndepVarsPredGrid
readIndepVarsPredGrid
    (SpaceTimeGridSettings
        inSpatGridFile
        inTempGrid
        inSpaceTimeFilter
        inSpatDistFile
        inObsTempSamplesFile
        noOrderCheck
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
            _       -> Just <$> readSpatDist (ReadSpatDistParse noOrderCheck observations inSpatGrid path)
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
                throw $ NormalException "spatiotemporal positions in --obsFile not readable, \
                                        \maybe wrong column names"
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
        throw $ NormalException "indep vars in --obsFile and --anyGridFile not equal"
    return $ ArbitraryDimGrid inArbitraryDimPos

readDepVarsPredGrid ::
       [DepVarsPos]
    -> V.Vector Observation
    -> IO DepVarsPredGrid
readDepVarsPredGrid
    depVarsPos
    observations = do
    -- input validation
    let varsFromObs  = getKeys $ (_hyposDepVarsPos . _obsPos) $ V.head observations
        varsFromGrid = getKeys $ head depVarsPos
    OP.when (varsFromObs /= varsFromGrid) $ do
        throw $ NormalException "dep vars in --obsFile and --depVars not equal"
    return $ DepVarsPredGrid depVarsPos

createCoreSupplement :: SearchGrid -> CoreSupplement
createCoreSupplement (SearchGrid indepVarsPredGrid _) =
    case indepVarsPredGrid of
        SpaceTimeGrid _ _ spaceTimeFilter maybeSpatDistMap maybeTempSamples ->
            CoreSupplement spaceTimeFilter maybeSpatDistMap maybeTempSamples
        ArbitraryDimGrid _ ->
            CoreSupplement Nothing Nothing Nothing

validateAlgorithmInterpol :: KernelDefinition -> IndepVarsPredGrid -> IO ()
validateAlgorithmInterpol
    (KernelDefinition kernelsPerDepVars)
    (SpaceTimeGrid {}) = do
        let allIndepVarsFromAlg = map (getKeys . _kodvLengths) kernelsPerDepVars
            indepVarsFromGrid = ["space", "time"]
        OP.unless (allEqual allIndepVarsFromAlg) $
            throw $ NormalException "indep var names not equal across kernel definitions"
        OP.unless (head allIndepVarsFromAlg == indepVarsFromGrid) $
            throw $ NormalException "indep vars not equal to \"space\" and \"time\""
validateAlgorithmInterpol
    (KernelDefinition kernelsPerDepVars)
    (ArbitraryDimGrid arbitraryDimPos) = do
        let allIndepVarsFromAlg = map (getKeys . _kodvLengths) kernelsPerDepVars
            indepVarsFromGrid = getKeys $ V.head  arbitraryDimPos
        OP.unless (allEqual allIndepVarsFromAlg) $
            throw $ NormalException "indep var names not equal across kernel definitions"
        OP.unless (head allIndepVarsFromAlg == indepVarsFromGrid) $
            throw $ NormalException "indep vars in --anyGridFile and --algorithm not equal"

validateAlgorithmSearch :: KernelDefinition -> DepVarsPredGrid -> IO ()
validateAlgorithmSearch
    kernelDef
    (DepVarsPredGrid depVarsPos) = do
        let depVarsFromAlg = getKeys kernelDef
            depVarsFromGrid = head $ map getKeys depVarsPos
        OP.unless (depVarsFromAlg == depVarsFromGrid) $
            throw $ NormalException "dep vars in --depVars and --algorithm not equal"

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

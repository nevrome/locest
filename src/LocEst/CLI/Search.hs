{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocEst.CLI.Search where

import           LocEst.CoreAlgorithms
import           LocEst.Parsers
import           LocEst.Types
import           LocEst.Utils

import qualified Codec.Serialise               as S
import           Conduit                       (MonadIO, liftIO)
import           Control.Exception             (throw)
import qualified Control.Monad                 as OP
import qualified Control.Monad.Except          as E
import           Data.Conduit                  ((.|))
import qualified Data.Conduit                  as Con
import qualified Data.Conduit.Algorithms.Async as ConAA
import qualified Data.Conduit.Combinators      as ConC
import qualified Data.Conduit.List             as ConL
import           GHC.Conc                      (getNumCapabilities)
import           System.IO                     (hPutStrLn, stderr)
import qualified Data.List.NonEmpty            as NE

data SearchOptions = SearchOptions
    { _searchInObservationFile  :: FilePath
    , _searchSearchGridSettings :: SearchGridSettings
    , _searchAlgorithm          :: LocestAlgorithm
    , _normalize                :: Normalization
    , _numThreads               :: NumberOfThreads
    , _searchOutFile            :: FilePath
    }

data SearchGridSettings = SearchGridSettings {
      _searchPosSetIndepVarsGrid :: IndepVarsPredGridSettings
    , _searchPosSetDepVarsGrid   :: [DepVarsPos]
}

data IndepVarsPredGridSettings = SpaceTimeGridSettings {
      _stgsInSpatGridFile       :: FilePath
    , _stgsInTempGrid           :: [Int]
    , _spaceSpaceTimeFilter     :: Maybe (Double,Double)
    , _stgsInSpatDistFile       :: Maybe FilePath
    , _stgsInObsTempSamplesFile :: Maybe FilePath
} | ArbitraryDimGridSettings {
      _adgsInArbitraryDimGridFile :: FilePath
}

runSearch :: SearchOptions -> IO ()
runSearch (
    SearchOptions
        inObsFile
        (SearchGridSettings indepVarsPredGridSettings depVarsPredGridSettings)
        algorithm
        normalization
        threads
        outFile
    ) = do

    -- number of threads
    numThreads <- case threads of
        SingleThread      -> pure 1
        MultipleThreads n -> pure n
        DetectThreads     -> do
            detectedThreads <- getNumCapabilities
            hPutStrLn stderr $ "Detected max number of threads: " ++ show detectedThreads
            return detectedThreads
    hPutStrLn stderr $ "Working with threads: " ++ show numThreads
    -- read observations
    hPutStrLn stderr "Reading observations"
    !observationsUnindexed <- readObservations inObsFile
    let observations = zipWith setIndex observationsUnindexed [0..]
    -- read and prepare prediction grids
    hPutStrLn stderr "Preparing prediction grid"
    indepVarsPredGrid <- readIndepVarsPredGrid indepVarsPredGridSettings observations
    depVarsPredGrid   <- readDepVarsPredGrid   depVarsPredGridSettings   observations
    validateAlgorithm algorithm indepVarsPredGrid depVarsPredGrid
    let searchGrid = SearchGrid indepVarsPredGrid depVarsPredGrid
        supplement = createCoreSupplement searchGrid
    -- validate algorithm settings
    -- prepare permutations
    hPutStrLn stderr "Preparing permutations"
    permutations <- createPermutations algorithm indepVarsPredGrid depVarsPredGrid

    hPutStrLn stderr "All preparations ready"

    -- run all permutations
    case permutations of
        Left e -> throw e
        Right perms -> do
            hPutStrLn stderr "Running analysis"
            -- run analysis pipeline
            Con.runConduitRes $
                ConL.sourceList perms
                -- main search algorithm
                -- 1. sequential
                -- .| ConL.map coreSearch
                -- 2. normal parallel
                .| ConAA.asyncMapC numThreads (E.runExcept . coreSearch observations supplement)
                -- 3. chunked parallel
                -- .| Con.conduitVector 100 .| ConAA.asyncMapC 5 (V.map coreSearch) .| ConL.concat
                -- print progress information
                .| progress 1000
                -- split stream to report the error cases and write the good results to the file system
                .| Con.getZipSink (
                        Con.ZipSink (
                               ConL.mapMaybe leftToJust
                            .| ConL.groupOn id
                            .| ConL.mapM_ printErrors
                            --    ConC.filter isLeft
                            -- .| ConL.mapM_ printError
                        ) *>
                        Con.ZipSink (
                               ConL.mapMaybe rightToJust
                            .| normalize normalization -- this assumes the permutation order to be set accordingly!!
                                                       -- otherwise sorting is necessary, which means everything has to go into memory
                            .| sinkNamedCSV outFile
                        )
                   )
            hPutStrLn stderr "Done"

printErrors :: MonadIO m => NE.NonEmpty LOCESTException -> m ()
printErrors errMsg = liftIO $ hPutStrLn stderr (show (length errMsg) ++ " * " ++ renderLOCESTException (NE.head errMsg))

readIndepVarsPredGrid ::
       IndepVarsPredGridSettings
    -> [Observation]
    -> IO IndepVarsPredGrid
readIndepVarsPredGrid
    (SpaceTimeGridSettings
        inSpatGridFile
        inTempGrid
        inSpaceTimeFilter
        inSpatDistFile
        inObsTempSamplesFile
    )
    observations = do
    hPutStrLn stderr "Assuming a spatiotemporal system"
    -- read spatial grid
    hPutStrLn stderr "Reading spatial grid positions"
    !inSpatGridUnindexed <- readSpatPos inSpatGridFile
    let inSpatGrid = zipWith setIndex inSpatGridUnindexed [0..]
    -- read spatial distances
    !inSpatDists <- case inSpatDistFile of
        Nothing   -> pure Nothing
        Just path -> do
            hPutStrLn stderr $ "Deserialising spatial distances from " ++ path
            dists <- S.readFileDeserialise path
            return $ Just dists
    -- read temporal distances
    !inObsTempSamples <- case inObsTempSamplesFile of
        Nothing   -> pure Nothing
        Just path -> do
            hPutStrLn stderr "Reading temporal resampling ages"
            Just <$> readTempSamp False observations path
    -- input validation
    case head $ map (_hyposIndepVarsPos . _obsPos) observations of
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
    hPutStrLn stderr "Reading arbitrary-dimension grid positions"
    !inArbitraryDimPos <- readArbitraryDimPos inArbitraryDimGridFile
    -- input validation
    let varsFromObs = case head $ map (_hyposIndepVarsPos . _obsPos) observations of
            IndepSpatTempPos _     -> []
            IndepArbitraryDimPos x -> getKeys x
    let varsFromGrid = getKeys $ head inArbitraryDimPos
    OP.when (varsFromObs /= varsFromGrid) $ do
        throw $ NormalException "indep vars in --obsFile and --anyGridFile not equal"
    return $ ArbitraryDimGrid inArbitraryDimPos

readDepVarsPredGrid ::
       [DepVarsPos]
    -> [Observation]
    -> IO DepVarsPredGrid
readDepVarsPredGrid
    depVarsPos
    observations = do
    -- input validation
    let varsFromObs  = getKeys $ (_hyposDepVarsPos . _obsPos) $ head observations
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

validateAlgorithm :: LocestAlgorithm -> IndepVarsPredGrid -> DepVarsPredGrid -> IO ()
validateAlgorithm
    (AlgoKernSmooth kernelDef@(KernelDefinition kernelsPerDepVars))
    (SpaceTimeGrid {})
    (DepVarsPredGrid depVarsPos) = do
        let depVarsFromAlg = getKeys kernelDef
            allIndepVarsFromAlg = map (getKeys . _kodvKernel) kernelsPerDepVars
            depVarsFromGrid = head $ map getKeys depVarsPos
            indepVarsFromGrid = ["space", "time"]
        OP.unless (allEqual allIndepVarsFromAlg) $
            throw $ NormalException "indep var names not equal across kernel definitions"
        OP.unless (depVarsFromAlg == depVarsFromGrid) $
            throw $ NormalException "dep vars in --depVars and --algorithm not equal"
        OP.unless (head allIndepVarsFromAlg == indepVarsFromGrid) $
            throw $ NormalException "indep vars not equal to \"space\" and \"time\""
validateAlgorithm
    (AlgoKernSmooth kernelDef@(KernelDefinition kernelsPerDepVars))
    (ArbitraryDimGrid arbitraryDimPos)
    (DepVarsPredGrid depVarsPos) = do
        let depVarsFromAlg = getKeys kernelDef
            allIndepVarsFromAlg = map (getKeys . _kodvKernel) kernelsPerDepVars
            depVarsFromGrid = head $ map getKeys depVarsPos
            indepVarsFromGrid = head $ map getKeys arbitraryDimPos
        OP.unless (allEqual allIndepVarsFromAlg) $
            throw $ NormalException "indep var names not equal across kernel definitions"
        OP.unless (depVarsFromAlg == depVarsFromGrid) $
            throw $ NormalException "dep vars in --depVars and --algorithm not equal"
        OP.unless (head allIndepVarsFromAlg == indepVarsFromGrid) $
            throw $ NormalException "indep vars in --anyGridFile and --algorithm not equal"

createPermutations ::
       LocestAlgorithm
    -> IndepVarsPredGrid
    -> DepVarsPredGrid
    -> IO (Either LOCESTException [CorePermutation])
createPermutations
    algorithm
    (SpaceTimeGrid inSpatGrid inTempGrid _ _ inObsTempSamples)
    (DepVarsPredGrid depVarPos) = do
        hPutStrLn stderr $ "Permutations: " ++ "\n" ++
            "   1 algorithm" ++ "\n" ++
            " * " ++ show nrTempSamples       ++ " time resampling iterations"   ++ "\n" ++
            " * " ++ show (length depVarPos)  ++ " dependent variable positions" ++ "\n" ++
            " * " ++ show (length inTempGrid) ++ " time slices"                  ++ "\n" ++
            " * " ++ show (length inSpatGrid) ++ " spatial positions"
        hPutStrLn stderr $ "Required iterations: " ++
            show (nrTempSamples * length depVarPos * length inTempGrid * length inSpatGrid)
        return $ Right replicateWithListMonad
        where
            nrTempSamples = case inObsTempSamples of
                Nothing                       -> 1
                Just (TempSampleMatrix n _ _) -> n
            replicateWithListMonad :: [CorePermutation]
            replicateWithListMonad = do
                tempSamp <- [0..(nrTempSamples-1)]
                depPos <- depVarPos
                tempPos <- inTempGrid
                spatPos <- inSpatGrid
                return $ CorePermutation
                            (HyperPos (IndepSpatTempPos (SpatTempPos spatPos (TempPos tempPos))) depPos)
                            algorithm
                            tempSamp
createPermutations
    algorithm
    (ArbitraryDimGrid gridPos)
    (DepVarsPredGrid depVarPos) = return $ Right replicateWithListMonad
        where
            replicateWithListMonad :: [CorePermutation]
            replicateWithListMonad = do
                indepPos <- gridPos
                depPos <- depVarPos
                return $ CorePermutation
                            (HyperPos (IndepArbitraryDimPos indepPos) depPos)
                            algorithm
                            0

normalize :: Monad m => Normalization -> Con.ConduitT SearchResult SearchResult m ()
normalize NoNorm = ConC.map id
normalize NormBySpace =
       ConL.groupBy groupingCriteria
    .| ConL.map scaleProbs
    .| ConL.concat
    where
    groupingCriteria :: SearchResult -> SearchResult -> Bool
    groupingCriteria
        (SearchResult (CorePermutation (HyperPos (IndepSpatTempPos (SpatTempPos _ t1)) dv1) alg1 tri1) _ _)
        (SearchResult (CorePermutation (HyperPos (IndepSpatTempPos (SpatTempPos _ t2)) dv2) alg2 tri2) _ _) =
            t1 == t2 && dv1 == dv2 && alg1 == alg2 && tri1 == tri2
    groupingCriteria _ _ = False
    scaleProbs :: [SearchResult] -> [SearchResult]
    scaleProbs stps =
        let probs = map _srProbability stps
            totalProb = sum probs
            rescaledProbs = map (/ totalProb) probs
        in zipWith setProb stps rescaledProbs
    setProb :: SearchResult -> Double -> SearchResult
    setProb stp p = stp {_srProbability = p}

allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual (x:xs) = all (== x) xs

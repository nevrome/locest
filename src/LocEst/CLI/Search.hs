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
import           Data.Conduit                  ((.|))
import qualified Data.Conduit                  as Con
import qualified Data.Conduit.Algorithms.Async as ConAA
import qualified Data.Conduit.Combinators      as ConC
import qualified Data.Conduit.List             as ConL
import           Data.Either                   (isLeft)
import           Data.Function                 ((&))
import qualified Data.HashMap.Strict           as HM
import           Data.List                     (sort)
import           GHC.Conc                      (getNumCapabilities)
import           System.IO                     (hPutStrLn, stderr)

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

readIndepVarsPredGrid :: IndepVarsPredGridSettings -> [Observation] -> IO IndepVarsPredGrid
readIndepVarsPredGrid
    (SpaceTimeGridSettings inSpatGridFile inTempGrid inSpaceTimeFilter inSpatDistFile inObsTempSamplesFile)
    observations = do
    !inSpatGridUnindexed <- readSpatPos inSpatGridFile
    let inSpatGrid = zipWith setIndex inSpatGridUnindexed [0..]
    !inSpatDists <- case inSpatDistFile of
        Nothing   -> pure Nothing
        Just path -> do
            hPutStrLn stderr $ "Deserialising spatial distances from " ++ path
            dists <- S.readFileDeserialise path
            return $ Just dists
    !inObsTempSamples <- case inObsTempSamplesFile of
        Nothing   -> pure Nothing
        Just path -> Just <$> readTempSamp False observations path
    return $ SpaceTimeGrid inSpatGrid inTempGrid inSpaceTimeFilter inSpatDists inObsTempSamples
readIndepVarsPredGrid
    (ArbitraryDimGridSettings inArbitraryDimGridFile)
    observations = do
    !inArbitraryDimPos <- readArbitraryDimPos inArbitraryDimGridFile
    let indepVarsFromObsOrdered = case (head $ map (_hyposIndepVarsPos . _obsPos) observations) of
            IndepSpatTempPos _     -> []
            IndepArbitraryDimPos x -> sort . HM.keys . getADPHM $ x
    let indepVarsPosFromGridOrdered = sort . HM.keys . getADPHM $ head inArbitraryDimPos
    OP.when (indepVarsFromObsOrdered /= indepVarsPosFromGridOrdered) $ do
        throw $ NormalException "indep vars in -? and -? not equal"
    return $ ArbitraryDimGrid inArbitraryDimPos indepVarsPosFromGridOrdered

readDepVarsPredGrid :: [DepVarsPos] -> [Observation] -> IO DepVarsPredGrid
readDepVarsPredGrid depVarsPos observations = do
    let depVarsFromGridOrdered = sort . HM.keys . getHM $ head depVarsPos
        depVarsFromObsOrdered = sort . HM.keys . getHM $ (_hyposDepVarsPos . _obsPos) $ head observations
    OP.when (depVarsFromObsOrdered /= depVarsFromGridOrdered) $ do
        throw $ NormalException "dep vars in -? and -? not equal"
    return $ DepVarsPredGrid depVarsPos depVarsFromObsOrdered

createCoreSupplement :: SearchGrid -> CoreSupplement
createCoreSupplement (SearchGrid indepVarsPredGrid (DepVarsPredGrid _ depVarsOrdered)) =
    case indepVarsPredGrid of
        SpaceTimeGrid _ _ spaceTimeFilter maybeSpatDistMap maybeTempSamples ->
            CoreSupplement [] depVarsOrdered spaceTimeFilter maybeSpatDistMap maybeTempSamples
        ArbitraryDimGrid _ indepVarsOrdered ->
            CoreSupplement indepVarsOrdered depVarsOrdered Nothing Nothing Nothing

createPermutations ::
       LocestAlgorithm
    -> IndepVarsPredGrid
    -> DepVarsPredGrid
    -> IO (Either LOCESTException [CorePermutation])
createPermutations
    algorithm
    (SpaceTimeGrid inSpatGrid inTempGrid _ _ inObsTempSamples)
    (DepVarsPredGrid depVarPos _) = do
        let nrTempSamples = case inObsTempSamples of
                Nothing                       -> 1
                Just (TempSampleMatrix n _ _) -> n
            permutations = PTRoot [] &
                -- the following elements can be ordered arbitrarily
                addPermutation [PEAlgorithm algorithm] &
                addPermutation (map PETempSampling [0..(nrTempSamples-1)]) &
                addPermutation (map PEDepVarsPos depVarPos) &
                addPermutation (map PETempPos inTempGrid) &
                addPermutation (map PESpatPos inSpatGrid) &
                harvest
        hPutStrLn stderr $ "Permutations: " ++
            "1 algorithm" ++ " * " ++
            show nrTempSamples ++ " time resampling iterations" ++ " * " ++
            show (length depVarPos) ++ " dependent variable positions" ++ " * " ++
            show (length inTempGrid) ++ " time slices" ++ " * " ++
            show (length inSpatGrid) ++ " spatial positions"
        hPutStrLn stderr $ "Required iterations: " ++
            show (nrTempSamples * length depVarPos * length inTempGrid * length inSpatGrid)
        return permutations
createPermutations
    algorithm
    (ArbitraryDimGrid gridPos _)
    (DepVarsPredGrid depVarPos _) = return $ Right replicateWithListMonad
        where
            replicateWithListMonad :: [CorePermutation]
            replicateWithListMonad = do
                indepPos <- gridPos
                depPos <- depVarPos
                return $ CorePermutation (HyperPos (IndepArbitraryDimPos indepPos) depPos) algorithm 1

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
    !observationsUnindexed <- readObservations inObsFile
    let observations = zipWith setIndex observationsUnindexed [0..]
    -- read and prepare prediction grids
    indepVarsPredGrid <- readIndepVarsPredGrid indepVarsPredGridSettings observations
    depVarsPredGrid   <- readDepVarsPredGrid depVarsPredGridSettings observations
    let searchGrid = SearchGrid indepVarsPredGrid depVarsPredGrid
        supplement = createCoreSupplement searchGrid
    -- preparing permutations
    hPutStrLn stderr "Building permutation tree"
    permutations <- createPermutations algorithm indepVarsPredGrid depVarsPredGrid
    hPutStrLn stderr "Done"
    -- running all permutations
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
                .| ConAA.asyncMapC numThreads (coreSearch observations supplement)
                -- 3. chunked parallel
                -- .| Con.conduitVector 100 .| ConAA.asyncMapC 5 (V.map coreSearch) .| ConL.concat
                -- print progress information
                .| progress 1000
                -- split stream to report the error cases and write the good results to the file system
                .| Con.getZipSink (
                        Con.ZipSink (
                               ConC.filter isLeft
                            .| ConL.mapM_ printError
                        ) *>
                        Con.ZipSink (
                               ConL.mapMaybe rightToJust
                            .| normalize normalization -- this assumes the permutation order to be set accordingly!!
                                                       -- otherwise sorting is necessary, which means everything has to go into memory
                            .| sinkNamedCSV outFile
                        )
                   )
            hPutStrLn stderr "Done"

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
            maxProb = maximum probs
            rescaledProbs = map (/ maxProb) probs
        in zipWith setProb stps rescaledProbs
    setProb :: SearchResult -> Double -> SearchResult
    setProb stp p = stp {_srProbability = p}

allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual (x:xs) = all (== x) xs

printError :: MonadIO m => Either LOCESTException a -> m ()
printError (Left errMsg) = liftIO $ hPutStrLn stderr (renderLOCESTException errMsg ++ "\n")
printError (Right _) = error "this should never happen"

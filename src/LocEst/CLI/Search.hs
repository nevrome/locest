{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LocEst.CLI.Search where

import           LocEst.CoreAlgorithms
import           LocEst.Parsers
import           LocEst.Types
import           LocEst.Utils

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
import LocEst.Types (Observation(_obsIndex), Identifiable (setIndex))

data SearchOptions = SearchOptions
    { _searchInObservationFile      :: FilePath
    , _searchSearchPositionSettings :: ConcretePositionSettings
    , _searchAlgorithm              :: LocestAlgorithm
    , _spaceSpaceTimeFilter         :: Maybe (Double,Double)
    , _normalize                    :: Normalization
    , _numThreads                   :: NumberOfThreads
    , _searchOutFile                :: FilePath
    }

data ConcretePositionSettings = ConcretePositionSettings {
      _concPosInSpatGridFile :: FilePath
    , _concPosInTempGrid     :: [Int]
    , _concPosDepVarsPosGrid :: [DepVarsPos]
    , _concPosSpatDistFile   :: Maybe FilePath
}

runSearch :: SearchOptions -> IO ()
runSearch (
    SearchOptions inObsFile
        (ConcretePositionSettings inSpatGridFile inTempGrid searchDepVarPos inSpatDistFile)
        algorithm
        spaceTimeFilter
        normalization
        threads
        outFile
    ) = do
    !allObservationsUnindexed <- readObservations inObsFile
    let allObservations = map (\(i,x) -> setIndex x i) $ zip 0:(length allObservationsUnindexed - 1) allObservationsUnindexed
    !inSpatGridUnindexed <- readSpatPos inSpatGridFile
    let allObservations = map (\(i,x) -> setIndex x i) $ zip 0:(length inSpatGridUnindexed - 1) inSpatGridUnindexed
    let depVarsOrdered = sort . HM.keys . getHM $ head $ map (_stpoDepVarsPos . _obsPos) allObservations
    let depVarsFromSearch = map (sort . HM.keys . getHM) searchDepVarPos
    !inSpatDists <- case inSpatDistFile of
        Nothing   -> return Nothing
        Just path -> Just <$> readSpatDist path
    -- validating input
    OP.when (not $ allEqual depVarsFromSearch) $ do
        throw $ NormalException "dep vars within -d not equal"
    OP.when (depVarsOrdered /= head depVarsFromSearch) $ do
        throw $ NormalException "dep vars in -i and -d not equal"
    -- number of threads
    numThreads <- case threads of
        SingleThread      -> pure 1
        MultipleThreads n -> pure n
        DetectThreads     -> do
            detectedThreads <- getNumCapabilities
            hPutStrLn stderr $ "Detected max number of threads: " ++ show detectedThreads
            return detectedThreads
    hPutStrLn stderr $ "Working with threads: " ++ show numThreads
    -- preparing permutations
    hPutStrLn stderr $ "Permutations: " ++
        "1 algorithm" ++ " * " ++
        show (length searchDepVarPos) ++ " dependent variable positions" ++ " * " ++
        show (length inTempGrid) ++ " time slices" ++ " * " ++
        show (length inSpatGrid) ++ " spatial positions"
    hPutStrLn stderr $ "Required iterations: " ++ show (length searchDepVarPos * length inTempGrid * length inSpatGrid)
    hPutStrLn stderr "Building permutation tree"
    let permutations =
            PTRoot [] &
            addPermutation (map PEAlgorithm [algorithm]) & -- can be ordered arbitrarily
            addPermutation (map PEDepVarsPos searchDepVarPos) &
            addPermutation (map PETempPos inTempGrid) &
            addPermutation (map PESpatPos inSpatGrid) &
            harvest
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
                .| ConAA.asyncMapC numThreads (coreSearch depVarsOrdered allObservations inSpatDists spaceTimeFilter)
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
        (SearchResult (SpatTempDepVarsPosWithAlgorithms (SpatTempDepVarsPos (SpatTempPos _ t1) dv1) alg1) _ _)
        (SearchResult (SpatTempDepVarsPosWithAlgorithms (SpatTempDepVarsPos (SpatTempPos _ t2) dv2) alg2) _ _) =
            t1 == t2 && dv1 == dv2 && alg1 == alg2
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

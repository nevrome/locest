{-# LANGUAGE ScopedTypeVariables #-}

module LocEst.CLI.Search where

import           LocEst.Parsers
import           LocEst.Types
import           LocEst.CoreAlgorithms

import           Data.Conduit                   ((.|))
import qualified Data.Conduit                   as Con
import qualified Data.Conduit.Algorithms.Async  as ConAA
import qualified Data.Conduit.List              as ConL

data SearchOptions = SearchOptions
    { _searchInObservationFile :: FilePath
    , _searchInSpatGridFile    :: FilePath
    , _searchInTempGrid        :: [Int]
    , _searchSearchDepVars     :: DepVarsMap
    , _searchOutFile           :: FilePath
    }

runSearch :: SearchOptions -> IO ()
runSearch (
    SearchOptions inObsFile inSpatGridFile inTempGrid searchDepVars outFile
    ) = do
    allObservations <- readSpatTempObs inObsFile
    Con.runConduitRes $
           sourceCSV inSpatGridFile
        -- multiply spatial input grid by temporal grid
        .| ConL.concatMap (multiplySpatPosByTempGrid inTempGrid)
        -- .| ConL.map myFunc -- sequential
        .| ConAA.asyncMapC 5 (myFunc searchDepVars allObservations) -- normal parallel
        -- .| Con.conduitVector 100 .| ConAA.asyncMapC 5 (V.map myFunc) .| ConL.concat -- chunked parallel
        .| progress
        .| sinkCSV outFile

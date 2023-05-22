{-# LANGUAGE ScopedTypeVariables #-}

module LocEst.CLI.Search where

import           LocEst.Parsers
import           LocEst.Types
import           LocEst.CoreAlgorithms
import LocEst.Utils

import           Data.Conduit                   ((.|))
import qualified Data.Conduit                   as Con
import qualified Data.Conduit.Algorithms.Async  as ConAA
import qualified Data.Conduit.List              as ConL
import qualified Data.HashMap.Strict            as HM
import Data.List (sort)
import qualified Control.Monad as OP
import Control.Exception (throw)

data SearchOptions = SearchOptions
    { _searchInObservationFile :: FilePath
    , _searchInSpatGridFile    :: FilePath
    , _searchInTempGrid        :: [Int]
    , _searchSearchDepVars     :: [DepVarsPos]
    , _searchOutFile           :: FilePath
    }

runSearch :: SearchOptions -> IO ()
runSearch (
    SearchOptions inObsFile inSpatGridFile inTempGrid searchDepVarPos outFile
    ) = do
    allObservations <- readSpatTempObs inObsFile
    let depVarsOrdered = sort . HM.keys . getHM $ head $ map _stpoDepVarsPos allObservations
    let depVarsFromSearch = map (sort . HM.keys . getHM) searchDepVarPos
    -- validate input
    OP.when (not $ allEqual depVarsFromSearch) $ do
        throw $ NormalException "dep vars within -d not equal"
    OP.when (depVarsOrdered /= head depVarsFromSearch) $ do
        throw $ NormalException "dep vars in -i and -d not equal"
    -- run analysis pipeline
    Con.runConduitRes $
           sourceCSV inSpatGridFile
        -- multiply spatial input grid by temporal grid
        .| ConL.concatMap (multiplySpatPosByTempGrid inTempGrid)
        -- .| ConL.map coreSearch -- sequential
        .| ConAA.asyncMapC 5 (coreSearch depVarsOrdered allObservations searchDepVarPos) -- normal parallel
        -- .| Con.conduitVector 100 .| ConAA.asyncMapC 5 (V.map coreSearch) .| ConL.concat -- chunked parallel
        .| progress
        .| ConL.concat
        .| sinkCSV outFile

allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual (x:xs) = all (== x) xs
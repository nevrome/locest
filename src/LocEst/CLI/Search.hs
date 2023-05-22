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
    let depVars = map (sort . HM.keys . getHM) searchDepVarPos
    OP.when (not $ allEqual depVars) $ do
        throw $ NormalException "dep vars in input -d not equal"
    allObservations <- readSpatTempObs inObsFile
    Con.runConduitRes $
           sourceCSV inSpatGridFile
        -- multiply spatial input grid by temporal grid
        .| ConL.concatMap (multiplySpatPosByTempGrid inTempGrid)
        -- .| ConL.map coreSearch -- sequential
        .| ConAA.asyncMapC 5 (coreSearch allObservations searchDepVarPos) -- normal parallel
        -- .| Con.conduitVector 100 .| ConAA.asyncMapC 5 (V.map coreSearch) .| ConL.concat -- chunked parallel
        .| progress
        .| ConL.concat
        .| sinkCSV outFile

allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual (x:xs) = all (== x) xs
{-# LANGUAGE ScopedTypeVariables #-}

module LocEst.CLI.Search where

import           LocEst.Parsers
import           LocEst.TypesPositions
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
import System.IO (hPutStrLn, stderr)

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
    allObservations <- readSpatTempDepVarsPos inObsFile
    inSpatGrid <- readSpatPos inSpatGridFile
    let depVarsOrdered = sort . HM.keys . getHM $ head $ map _stpoDepVarsPos allObservations
    let depVarsFromSearch = map (sort . HM.keys . getHM) searchDepVarPos
    -- validate input
    OP.when (not $ allEqual depVarsFromSearch) $ do
        throw $ NormalException "dep vars within -d not equal"
    OP.when (depVarsOrdered /= head depVarsFromSearch) $ do
        throw $ NormalException "dep vars in -i and -d not equal"
    -- info
    hPutStrLn stderr $ "Required iterations: " ++
        show (length inSpatGrid) ++ " spatial positions"
        ++ " * " ++
        show (length inTempGrid) ++ " time slices"
        ++ " * " ++
        show (length searchDepVarPos) ++ " dependent variable positions"
        ++ " * " ++
        show (length myDecays) ++ " decay definitions"
        ++ " * " ++
        show (length myDecays) ++ " summary algorithms"
    -- run analysis pipeline
    Con.runConduitRes $
           ConL.sourceList inSpatGrid
        -- multiply spatial input grid by temporal grid
        .| ConL.concatMap (multiplySpatPosByTempGrid inTempGrid)
        -- multiply spatpos input grid by dependent vars positions
        .| ConL.concatMap (multiplySpatPosByDepVarsPos searchDepVarPos)
        -- multiply multidimensional positions by algorithms
        .| ConL.concatMap (multiplySpatTempDepVarsPosByAlgorithms myDecays mySummaries)
        -- .| ConL.map coreSearch -- sequential
        .| ConAA.asyncMapC 5 (coreSearch depVarsOrdered allObservations) -- normal parallel
        -- .| Con.conduitVector 100 .| ConAA.asyncMapC 5 (V.map coreSearch) .| ConL.concat -- chunked parallel
        .| progress
        .| sinkNamedCSV outFile

allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual (x:xs) = all (== x) xs

-- Idea: DensitySummaryAlgorithm and DecayDefinition should be list input arguments (with sequence option for numerics)
--       These lists can then be multiplied into the analysis loop, just as for tempGrid and the DepVarsPos
--       Crossvalidation specification involves then an alternative (!) to --spatGridFile
--       This specification probably has to include test:training split ratio and iterations

mySummaries = [mySummary]

myDecays = [myDecay]

mySummary = Maximum

myDecay = DecayDefinition [
      DecayOneDepVar "varC1" (LinearSum 0.0001 0.0001)
    , DecayOneDepVar "varC2" (LinearSum 0.0001 0.0001)
    ]

multiplySpatPosByTempGrid :: [Int] -> SpatPos -> [SpatTempPos]
multiplySpatPosByTempGrid tempGrid spatPos =
    map (\y -> SpatTempPos { _spatialPos = spatPos, _temporalPos = SimpleYearBCAD y}) tempGrid

multiplySpatPosByDepVarsPos :: [DepVarsPos] -> SpatTempPos -> [SpatTempDepVarsPos]
multiplySpatPosByDepVarsPos depVarsPos spatTempPos =
    map (\p -> SpatTempDepVarsPos { _stpoSpatTempPos = spatTempPos, _stpoDepVarsPos = p}) depVarsPos

multiplySpatTempDepVarsPosByAlgorithms ::
       [DecayDefinition]
    -> [DensitySummaryAlgorithm]
    -> SpatTempDepVarsPos
    -> [(SpatTempDepVarsPos, DecayDefinition, DensitySummaryAlgorithm)]
multiplySpatTempDepVarsPosByAlgorithms 
    decayDefinitions
    densitySummaryAlgorithms
    spatTempDepVarsPos =
    [ (spatTempDepVarsPos,x,y) | x <- decayDefinitions, y <- densitySummaryAlgorithms ] 




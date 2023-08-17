{-# LANGUAGE ScopedTypeVariables #-}

module LocEst.CLI.Search where

import           LocEst.CoreAlgorithms
import           LocEst.Parsers
import           LocEst.Types
import           LocEst.Utils

import           Conduit                       (liftIO)
import           Control.Exception             (throw)
import qualified Control.Monad                 as OP
import           Data.Conduit                  ((.|))
import qualified Data.Conduit                  as Con
import qualified Data.Conduit.Algorithms.Async as ConAA
import qualified Data.Conduit.Combinators      as ConC
import qualified Data.Conduit.List             as ConL
import           Data.Either                   (isLeft, isRight, fromRight)
import qualified Data.HashMap.Strict           as HM
import           Data.List                     (sort)
import           GHC.Conc                      (getNumCapabilities)
import           System.IO                     (hPutStrLn, stderr)
import Data.ByteString (hPut)
import Data.Function ((&))

data SearchOptions = SearchOptions
    { _searchInObservationFile      :: FilePath
    , _searchSearchPositionSettings :: ConcretePositionSettings
    , _searchAlgorithm              :: LocestAlgorithm
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
        outFile
    ) = do
    allObservations <- readObservations inObsFile
    inSpatGrid <- readSpatPos inSpatGridFile
    let depVarsOrdered = sort . HM.keys . getHM $ head $ map (_stpoDepVarsPos . _obsPos) allObservations
    let depVarsFromSearch = map (sort . HM.keys . getHM) searchDepVarPos
    inSpatDists <- case inSpatDistFile of
        Nothing   -> return Nothing
        Just path -> Just <$> readSpatDist path
    -- validate input
    OP.when (not $ allEqual depVarsFromSearch) $ do
        throw $ NormalException "dep vars within -d not equal"
    OP.when (depVarsOrdered /= head depVarsFromSearch) $ do
        throw $ NormalException "dep vars in -i and -d not equal"
    -- info
    maxNumberOfThreads <- getNumCapabilities
    hPutStrLn stderr $ "Detected max number of threads: " ++ show maxNumberOfThreads
    hPutStrLn stderr $ "Required iterations: " ++
        show (length inSpatGrid) ++ " spatial positions"
        ++ " * " ++
        show (length inTempGrid) ++ " time slices"
        ++ " * " ++
        show (length searchDepVarPos) ++ " dependent variable positions"
    
    let permutations = PTRoot [] &
            addToTree (map PEAlgorithm [algorithm]) & -- can be ordered arbitrarily
            addToTree (map PEDepVarsPos searchDepVarPos) &
            addToTree (map PETempPos inTempGrid) &
            addToTree (map PESpatPos inSpatGrid) &
            harvestRipeTree

    case permutations of
        Left e -> throw e
        Right perms -> 
            -- run analysis pipeline
            Con.runConduitRes $
                -- begin to stream spatial prediction grid positions
                --   ConL.sourceList inSpatGrid
                -- multiply spatial input grid by temporal grid
                -- .| ConL.concatMap (multiplySpatPosByTempGrid inTempGrid)
                -- multiply spatpos input grid by dependent vars positions
                -- .| ConL.concatMap (multiplySpatPosByDepVarsPos searchDepVarPos)
                -- multiply multidimensional positions by algorithms (currently only one in input)
                -- .| ConL.concatMap (multiplySpatTempDepVarsPosByAlgorithms [algorithm])
                ConL.sourceList perms
                -- main search algorithm
                -- 1. sequential
                -- .| ConL.map coreSearch
                -- 2. normal parallel
                .| ConAA.asyncMapC maxNumberOfThreads (coreSearch depVarsOrdered allObservations inSpatDists)
                -- 3. chunked parallel
                -- .| Con.conduitVector 100 .| ConAA.asyncMapC 5 (V.map coreSearch) .| ConL.concat
                -- print progress information
                .| progress
                -- split stream to report the error cases and write the good results to the file system
                .| Con.getZipSink (
                        Con.ZipSink (
                               ConC.filter isLeft
                            .| ConL.mapM_ (\(Left errMsg) -> liftIO $ hPutStrLn stderr (renderLOCESTException errMsg ++ "\n"))
                        ) *>
                        Con.ZipSink (
                               ConL.mapMaybe rightToJust
                            .| sinkNamedCSV outFile
                        )
                   )

allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual (x:xs) = all (== x) xs

-- Idea: DensitySummaryAlgorithm and DecayDefinition should be list input arguments (with sequence option for numerics)
--       These lists can then be multiplied into the analysis loop, just as for tempGrid and the DepVarsPos
--       Crossvalidation specification involves then an alternative (!) to --spatGridFile
--       This specification probably has to include test:training split ratio and iterations

multiplySpatPosByTempGrid :: [Int] -> SpatPos -> [SpatTempPos]
multiplySpatPosByTempGrid tempGrid spatPos =
    map (\y -> SpatTempPos { _spatialPos = spatPos, _temporalPos = SimpleYearBCAD y}) tempGrid

multiplySpatPosByDepVarsPos :: [DepVarsPos] -> SpatTempPos -> [SpatTempDepVarsPos]
multiplySpatPosByDepVarsPos depVarsPos spatTempPos =
    map (\p -> SpatTempDepVarsPos { _stpoSpatTempPos = spatTempPos, _stpoDepVarsPos = p}) depVarsPos

multiplySpatTempDepVarsPosByAlgorithms ::
       [LocestAlgorithm]
    -> SpatTempDepVarsPos
    -> [SpatTempDepVarsPosWithAlgorithms]
multiplySpatTempDepVarsPosByAlgorithms
    algorithms
    spatTempDepVarsPos =
    map (\a -> SpatTempDepVarsPosWithAlgorithms spatTempDepVarsPos a) algorithms

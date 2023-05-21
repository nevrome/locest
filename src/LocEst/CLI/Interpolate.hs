{-# LANGUAGE ScopedTypeVariables #-}

module LocEst.CLI.Interpolate where

import           LocEst.Parsers
import           LocEst.Types
import           LocEst.CoreAlgorithms

import           Data.Conduit                   ((.|))
import qualified Data.Conduit                   as Con
import qualified Data.Conduit.Algorithms.Async  as ConAA
import qualified Data.Conduit.List              as ConL

data InterpolateOptions = InterpolateOptions
    { _interpolateInObservationFile :: FilePath
    , _interpolateInSpatGridFile    :: FilePath
    , _interpolateInTempGrid        :: [Int]
    , _interpolateSearchDepVars     :: DepVarsMap
    , _interpolateOutFile           :: FilePath
    }

runInterpolate :: InterpolateOptions -> IO ()
runInterpolate (
    InterpolateOptions inObsFile inSpatGridFile inTempGrid searchDepVars outFile
    ) = do
    allObservations <- readSpatTempObs inObsFile
    Con.runConduitRes $
           sourceCSV inSpatGridFile
        -- multiply spatial input grid by temporal grid
        .| ConL.concatMap (multiplySpatPosByTempGrid inTempGrid)
        .| ConAA.asyncMapC 5 (coreInterpolate searchDepVars allObservations) -- normal parallel
        .| progress
        .| sinkCSV outFile

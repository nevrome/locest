{-# LANGUAGE ScopedTypeVariables #-}

module LocEst.CLI.Crossvalidate where

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
import GHC.Conc (getNumCapabilities)

data CrossvalidateOptions = CrossvalidateOptions
    { _crossvalidateInObservationFile :: FilePath
    , _crossvalidateSettings          :: CrossvalidationSettings
    , _crossvalidateOutFile           :: FilePath
    }

data CrossvalidationSettings = CrossvalidationSettings {
      _crossvalTestFraction  :: Double
    , _crossvalIterations    :: Int
}

runCrossvalidate :: CrossvalidateOptions -> IO ()
runCrossvalidate (
    CrossvalidateOptions inObsFile (CrossvalidationSettings testFraction iterations) outFile
    ) = do
    putStrLn "undefined"


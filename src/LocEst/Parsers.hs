module LocEst.Parsers where

import LocEst.Types

import qualified Data.Csv                             as Csv
import qualified Data.Csv.Conduit                     as ConCsv
import Data.Conduit                         ((.|))
import qualified Data.Conduit                         as Con
import qualified Data.Conduit.Combinators             as Con
import           Data.Char                            (ord)
import qualified Data.Conduit.List as ConL

-- helper functions
decodingOptions :: Csv.DecodeOptions
decodingOptions = Csv.defaultDecodeOptions {
    Csv.decDelimiter = fromIntegral (ord '\t')
}

encodingOptions :: Csv.EncodeOptions
encodingOptions = Csv.defaultEncodeOptions {
      Csv.encDelimiter = fromIntegral (ord '\t')
    }

readSpatTempObs :: FilePath -> IO [SpatTempObs]
readSpatTempObs path =
    Con.runConduitRes $
           Con.sourceFile path
        .| ConCsv.fromNamedCsvLiftError (userError . show) decodingOptions
        .| ConL.map spatTempObsFromTsvRow
        .| ConL.consume

pipeSpatTempPosConduit :: FilePath -> FilePath -> IO ()
pipeSpatTempPosConduit inPath outPath =
    Con.runConduitRes $
           Con.sourceFile inPath
        .| ConCsv.fromNamedCsvLiftError (userError . show) decodingOptions
        .| ConL.map spatTempPosFromTsvRow
        .| ConL.map spatTempPosToTsvRow
        .| ConCsv.toCsv encodingOptions
        .| Con.sinkFile outPath


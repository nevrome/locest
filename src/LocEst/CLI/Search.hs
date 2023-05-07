{-# LANGUAGE ScopedTypeVariables   #-}

module LocEst.CLI.Search where

import LocEst.Types
import qualified Data.Csv                             as Csv
import qualified Data.Csv.Conduit                     as ConCsv
import Data.Conduit                         ((.|))
import qualified Data.Conduit                         as Con
import qualified Data.Conduit.Combinators             as Con
import           Data.Char                            (ord)
import qualified Data.Conduit.List as ConL

data SearchOptions = SearchOptions
    { _searchInObservationFile :: FilePath
    , _searchInSearchPosFile   :: FilePath
    , _searchOutFile           :: FilePath
    }

runSearch :: SearchOptions -> IO ()
runSearch (
    SearchOptions inObsFile inSearchPosFile outFile
    ) = do
    putStrLn $ inObsFile ++ "; " ++ inSearchPosFile ++ "; " ++ outFile

    (hu :: [SpatTempObsTsvRow]) <- Con.runConduitRes $
        Con.sourceFile inObsFile
        .| ConCsv.fromNamedCsvLiftError (userError . show) decodingOptions
        .| ConL.consume
    
    print hu



decodingOptions :: Csv.DecodeOptions
decodingOptions = Csv.defaultDecodeOptions {
    Csv.decDelimiter = fromIntegral (ord '\t')
}
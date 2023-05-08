{-# LANGUAGE ScopedTypeVariables   #-}

module LocEst.CLI.Search where

--import LocEst.Types
import LocEst.Parsers

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

    hu <- readSpatTempObs inObsFile

    print hu

    pipeSpatTempPosConduit inObsFile outFile


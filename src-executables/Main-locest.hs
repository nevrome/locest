{-# LANGUAGE OverloadedStrings #-}

--import           Paths_locest                     (version)

import           LocEst.CLI.Interface
import           LocEst.CLI.Search    (SearchOptions (..), runSearch)
import LocEst.Utils
import LocEst.CLI.Crossvalidate (CrossvalidateOptions (..), runCrossvalidate)

import           Control.Exception    (catch)
import           Data.Version         (Version, makeVersion, showVersion)
import qualified Options.Applicative  as OP
import           System.Exit          (exitFailure)
import           System.IO            (hPutStrLn, stderr)
import System.Environment (getArgs)

version :: Version
version = makeVersion [0,0,0]

-- data types
data Options = Options { _subcommand :: Subcommand }

data Subcommand =
      CmdSearch SearchOptions
    | CmdCrossvalidate CrossvalidateOptions

-- CLI interface configuration
main :: IO ()
main = do
    hPutStrLn stderr $ "locest v" ++ showVersion version
    hPutStrLn stderr ""
    -- read command line arguments from cmd and file
    rawCmdArgs <- getArgs
    hPutStrLn stderr $ show rawCmdArgs
    mergedCmdArgs <- case getConfigFilePath rawCmdArgs of
        Nothing -> do
            return rawCmdArgs
        Just configFilePath -> do
            parseConfigFile configFilePath
    -- parse arguments
    (Options subcommand) <- OP.handleParseResult $
                                OP.execParserPure (OP.prefs OP.showHelpOnEmpty) optParserInfo mergedCmdArgs
    -- run subcommand
    catch (runCmd subcommand) handler
    where
        getConfigFilePath :: [String] -> Maybe FilePath
        getConfigFilePath [] = Nothing
        getConfigFilePath ("--configFile" : path : _) = Just path
        getConfigFilePath (_ : xs) = getConfigFilePath xs

        handler :: LOCESTException -> IO ()
        handler e = do
            hPutStrLn stderr $ renderLOCESTException e
            exitFailure

runCmd :: Subcommand -> IO ()
runCmd o = case o of
    CmdSearch opts -> runSearch opts
    CmdCrossvalidate opts -> runCrossvalidate opts

optParserInfo :: OP.ParserInfo Options
optParserInfo = OP.info (OP.helper <*> versionOption <*> (Options <$> subcommandParser)) (
    OP.briefDesc <>
    OP.progDesc "..."
    )

versionOption :: OP.Parser (a -> a)
versionOption = OP.infoOption (showVersion version) (OP.long "version" <> OP.help "Show version")

subcommandParser :: OP.Parser Subcommand
subcommandParser = OP.subparser (
           OP.command "search" searchOptInfo
        <> OP.command "crossvalidate" crossvalidateOptInfo
    )
    where
        searchOptInfo = OP.info (OP.helper <*> (CmdSearch <$> searchOptParser))
            (OP.progDesc "Search...")
        crossvalidateOptInfo = OP.info (OP.helper <*> (CmdCrossvalidate <$> crossvalidateOptParser))
            (OP.progDesc "Crossvalidate...")

searchOptParser :: OP.Parser SearchOptions
searchOptParser = SearchOptions <$>
                            optParseInObservationFile
                        <*> optParseConcretePositionSettings
                        <*> optParseOutFile

crossvalidateOptParser :: OP.Parser CrossvalidateOptions
crossvalidateOptParser = CrossvalidateOptions <$>
                            optParseInObservationFile
                        <*> optParseCrossvalidationSettings
                        <*> optParseOutFile
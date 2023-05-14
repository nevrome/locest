{-# LANGUAGE OverloadedStrings #-}

--import           Paths_locest                     (version)

import LocEst.CLI.Interface
import LocEst.CLI.Search (SearchOptions (..), runSearch)

import           Control.Exception                  (catch, Exception)
import           Data.Version                       (showVersion, makeVersion, Version)
import qualified Options.Applicative                as OP
import           System.Exit                        (exitFailure)
import           System.IO                          (hPutStrLn, stderr)

version :: Version
version = makeVersion [0,0,0]

-- | Different exceptions for locest
data LOCESTException =
      TestException String -- ^ An exception to ...
    | TestException2 String -- ^ An exception to ...
    deriving (Show)

renderLOCESTException :: LOCESTException -> String 
renderLOCESTException (TestException s) = 
    "<!> Error: " ++ s
renderLOCESTException (TestException2 s) =
    "<!> Error: " ++ s

instance Exception LOCESTException

-- data types
data Options = Options { _subcommand :: Subcommand }

data Subcommand = CmdSearch SearchOptions

-- CLI interface configuration
main :: IO ()
main = do
    hPutStrLn stderr $ "locest v" ++ showVersion version
    hPutStrLn stderr ""
    (Options subcommand) <- OP.customExecParser (OP.prefs OP.showHelpOnEmpty) optParserInfo
    catch (runCmd subcommand) handler
    where
        handler :: LOCESTException -> IO ()
        handler e = do
            hPutStrLn stderr $ renderLOCESTException e
            exitFailure

runCmd :: Subcommand -> IO ()
runCmd o = case o of
    CmdSearch opts -> runSearch opts

optParserInfo :: OP.ParserInfo Options
optParserInfo = OP.info (OP.helper <*> versionOption <*> (Options <$> subcommandParser)) (
    OP.briefDesc <>
    OP.progDesc "..."
    )

versionOption :: OP.Parser (a -> a)
versionOption = OP.infoOption (showVersion version) (OP.long "version" <> OP.help "Show version")

subcommandParser :: OP.Parser Subcommand
subcommandParser = OP.subparser (OP.command "search" searchOptInfo)
    where
        searchOptInfo = OP.info (OP.helper <*> (CmdSearch <$> searchOptParser))
            (OP.progDesc "Search...")

searchOptParser :: OP.Parser SearchOptions
searchOptParser = SearchOptions <$> optParseInObservationFile
                                <*> optParseInSpatGridFile
                                <*> optParseTempGridString
                                <*> optParseOutFile
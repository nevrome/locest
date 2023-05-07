{-# LANGUAGE OverloadedStrings #-}

--import           Paths_locest                     (version)

import           Control.Exception                  (catch, Exception)
import           Data.Version                       (showVersion, makeVersion, Version)
import qualified Options.Applicative                as OP
import           System.Exit                        (exitFailure)
import           System.IO                          (hPutStrLn, stderr)

version :: Version
version = makeVersion [0,0,0]

-- data types
data LOCESToptions = LOCESToptions Bool

data Options = CmdLOCEST LOCESToptions

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

-- CLI interface configuration
main :: IO ()
main = do
    hPutStrLn stderr $ "locest v" ++ showVersion version
    -- prepare input parsing
    cmdOpts <- OP.customExecParser p optParserInfo
    catch (runCmd cmdOpts) handler
    where
        p = OP.prefs OP.showHelpOnEmpty
        handler :: LOCESTException -> IO ()
        handler e = do
            hPutStrLn stderr $ renderLOCESTException e
            exitFailure

runCmd :: Options -> IO ()
runCmd o = case o of
    CmdLOCEST opts -> runLOCEST opts

optParserInfo :: OP.ParserInfo Options
optParserInfo = OP.info (OP.helper <*> versionOption <*> optParser) (
    OP.briefDesc <>
    OP.progDesc "..."
    )

versionOption :: OP.Parser (a -> a)
versionOption = OP.infoOption (showVersion version) (OP.long "version" <> OP.help "Show version")

optParser :: OP.Parser Options
optParser = CmdLOCEST <$> locestOptParser

locestOptParser :: OP.Parser LOCESToptions
locestOptParser = LOCESToptions <$> optParseQuiet

optParseQuiet :: OP.Parser Bool
optParseQuiet = OP.switch (
    OP.long "quiet" <> 
    OP.short 'q' <>
    OP.help "Suppress the printing of ..."
    )


-----

runLOCEST :: LOCESToptions -> IO ()
runLOCEST _ = putStrLn "huhu"


{-# LANGUAGE OverloadedStrings #-}

import           LocEst.CLI.Interface
import           LocEst.CLI.Search    (SearchOptions (..), runSearch)
import           LocEst.CLI.Serialise (SerialiseOptions (..),
                                       SpatDistFileSettings (..), runSerialise)
import           LocEst.CLI.Vario     (VarioOptions (VarioOptions), runVario)
import           LocEst.Utils

import           Control.Exception    (catch)
import           Data.List            (isInfixOf)
import           Data.Version         (showVersion)
import qualified Options.Applicative  as OP
import           Paths_locest         (version)
import           System.Environment   (getArgs)
import           System.Exit          (exitFailure)
import           System.IO            (hPutStrLn, stderr)

-- data types
data Options = Options { _subcommand :: Subcommand }

data Subcommand =
      CmdSerialise SerialiseOptions
    | CmdSearch SearchOptions
    | CmdVario VarioOptions

-- CLI interface configuration
main :: IO ()
main = do
    hPutStrLn stderr $ "locest v" ++ showVersion version
    hPutStrLn stderr ""
    -- read command line arguments from cmd and potentially a config file
    rawCmdArgs <- getArgs
    mergedCmdArgs <- case getConfigFilePath rawCmdArgs of
        Nothing -> do
            let cmdArgs = removeConfigFileArg rawCmdArgs
            return cmdArgs
        Just configFilePath -> do
            let cmdArgs = removeConfigFileArg rawCmdArgs
            configFileArgs <- catch (parseConfigFile configFilePath) handler
            return $ cmdArgs ++ configFileArgs
    -- parse arguments
    (Options subcommand) <-
        OP.handleParseResult $
            OP.execParserPure (OP.prefs OP.showHelpOnEmpty) optParserInfo mergedCmdArgs
    -- run requested subcommand
    catch (runCmd subcommand) handler
    where
        -- handling the special --configFile argument
        getConfigFilePath :: [String] -> Maybe FilePath
        getConfigFilePath []                          = Nothing
        getConfigFilePath ("--configFile" : path : _) = Just path
        getConfigFilePath (_ : xs)                    = getConfigFilePath xs
        removeConfigFileArg :: [String] -> [String]
        removeConfigFileArg [] = []
        removeConfigFileArg (x : xs)
          | "--configFile" `isInfixOf` x = dropNextElement xs
          | otherwise                    = x : removeConfigFileArg xs
          where
            dropNextElement []       = []
            dropNextElement [_]      = []
            dropNextElement (_ : ys) = removeConfigFileArg ys
        -- exception handler
        handler :: LOCESTException -> IO a
        handler e = do
            hPutStrLn stderr $ renderLOCESTException e
            exitFailure

runCmd :: Subcommand -> IO ()
runCmd o = case o of
    CmdSerialise opts -> runSerialise opts
    CmdSearch opts    -> runSearch opts
    CmdVario opts     -> runVario opts

optParserInfo :: OP.ParserInfo Options
optParserInfo = OP.info (OP.helper <*> versionOption <*> (Options <$> subcommandParser)) (
    OP.briefDesc <>
    OP.progDesc "Spatiotemporal interpolation and search for macroscale archaeological data."
    )

versionOption :: OP.Parser (a -> a)
versionOption = OP.infoOption (showVersion version) (OP.long "version" <> OP.help "Show version")

subcommandParser :: OP.Parser Subcommand
subcommandParser = OP.subparser (
           OP.command "serialise" serialiseOptInfo
        <> OP.command "search" searchOptInfo
        <> OP.command "vario" varioOptInfo
    )
    where
        serialiseOptInfo = OP.info (OP.helper <*> (CmdSerialise <$> serialiseOptParser))
            (OP.progDesc "Transform input data to compact binary blobs for faster startup.")
        searchOptInfo = OP.info (OP.helper <*> (CmdSearch <$> searchOptParser))
            (OP.progDesc "Interpolate dependent variables in space and time to determine areas of \
                          \ increased similarity to specific observations.")
        varioOptInfo = OP.info (OP.helper <*> (CmdVario <$> varioOptParser))
            (OP.progDesc "...")

serialiseOptParser :: OP.Parser SerialiseOptions
serialiseOptParser = SerialiseSpatDistFile <$> (
                        SpatDistFileSettings <$>
                            optParseInSpatDistMapFile
                        <*> optParseInObservationFile
                        <*> optParseInSpatGridFile
                        <*> optParseInSpatDistNoOrderCheck
                        <*> optParseOutFile
                        )

searchOptParser :: OP.Parser SearchOptions
searchOptParser = SearchOptions <$>
                            optParseInObservationFile
                        <*> optParseSearchGridSettings
                        <*> optParseAlgorithmString
                        <*> optParseNormalization
                        <*> optParseNumberOfThreads
                        <*> optParseOutFile

varioOptParser :: OP.Parser VarioOptions
varioOptParser = VarioOptions <$>
                            optParseInObservationFile
                        <*> optParseVariogramOutFile

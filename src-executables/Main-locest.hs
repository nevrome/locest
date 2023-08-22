{-# LANGUAGE OverloadedStrings #-}

import           LocEst.CLI.Crossvalidate (CrossvalidateOptions (..),
                                           runCrossvalidate)
import           LocEst.CLI.Interface
import           LocEst.CLI.Search        (SearchOptions (..), runSearch)
import           LocEst.Utils

import           Control.Exception        (catch)
import           Data.List                (isInfixOf)
import           Data.Version             (showVersion)
import qualified Options.Applicative      as OP
import           Paths_locest             (version)
import           System.Environment       (getArgs)
import           System.Exit              (exitFailure)
import           System.IO                (hPutStrLn, stderr)

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
    -- read command line arguments from cmd and potentially a config file
    rawCmdArgs <- getArgs
    mergedCmdArgs <- case getConfigFilePath rawCmdArgs of
        Nothing -> do
            let cmdArgs = removeConfigFileArg rawCmdArgs
            return cmdArgs
        Just configFilePath -> do
            let cmdArgs = removeConfigFileArg rawCmdArgs
            configFileArgs <- parseConfigFile configFilePath
            --hPutStrLn stderr $ show $ cmdArgs ++ configFileArgs
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
          | otherwise                  = x : removeConfigFileArg xs
          where
            dropNextElement []       = []
            dropNextElement [_]      = []
            dropNextElement (_ : ys) = removeConfigFileArg ys
        -- exception handler
        handler :: LOCESTException -> IO ()
        handler e = do
            hPutStrLn stderr $ renderLOCESTException e
            exitFailure

runCmd :: Subcommand -> IO ()
runCmd o = case o of
    CmdSearch opts        -> runSearch opts
    CmdCrossvalidate opts -> runCrossvalidate opts

optParserInfo :: OP.ParserInfo Options
optParserInfo = OP.info (OP.helper <*> versionOption <*> (Options <$> subcommandParser)) (
    OP.briefDesc <>
    OP.progDesc "Spatiotemporal interpolation and search for macroscale archaeological data."
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
            (OP.progDesc "Interpolate dependent variables in space and time to determine areas of \
                          \ increased similarity to specific observations.")
        crossvalidateOptInfo = OP.info (OP.helper <*> (CmdCrossvalidate <$> crossvalidateOptParser))
            (OP.progDesc "Compare hyperparameter settings for the interpolation through crossvalidation.")

searchOptParser :: OP.Parser SearchOptions
searchOptParser = SearchOptions <$>
                            optParseInObservationFile
                        <*> optParseConcretePositionSettings
                        <*> optParseAlgorithmString
                        <*> optParseOutFile

crossvalidateOptParser :: OP.Parser CrossvalidateOptions
crossvalidateOptParser = CrossvalidateOptions <$>
                            optParseInObservationFile
                        <*> optParseCrossvalidationSettings
                        <*> optParseOutFile

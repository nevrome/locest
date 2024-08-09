{-# LANGUAGE OverloadedStrings #-}

import           LocEst.CLI.Cross     (CrossOptions (..), runCross)
import           LocEst.CLI.Interface
import           LocEst.CLI.Search    (SearchOptions (..), runSearch)
import           LocEst.CLI.Serialise (SerialiseOptions (..), runSerialise)
import           LocEst.CLI.Utils     (setNumberOfThreads)
import           LocEst.CLI.Vario     (VarioOptions (..), runVario)
import           LocEst.Exceptions

import           Control.Exception    (catch)
import           Data.List            (isInfixOf)
import           Data.Version         (showVersion)
import qualified Options.Applicative  as OP
import           Paths_locest         (version)
import           System.Environment   (getArgs)
import           System.Exit          (exitFailure)
import           System.IO            (hPutStrLn, stderr)
import           System.IO.Silently   (hSilence)

-- data types
data Options = Options {
      _subcommand          :: Subcommand
    , _quiet               :: Bool
    , _spatDistUnitScaling :: Double
    }

data Subcommand =
      CmdSerialise SerialiseOptions
    | CmdSearch SearchOptions
    | CmdVario VarioOptions
    | CmdCross CrossOptions

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
    (Options subcommand quiet spatDistUnitScaling) <-
        OP.handleParseResult $
            OP.execParserPure (OP.prefs OP.showHelpOnEmpty) optParserInfo mergedCmdArgs
    -- number of threads
    numThreads <- setNumberOfThreads
    -- run requested subcommand
    catch (run subcommand numThreads quiet spatDistUnitScaling) handler
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
        handler :: LocEstException -> IO a
        handler e = do
            hPutStrLn stderr $ renderLocEstException e
            exitFailure

run :: Subcommand -> Int -> Bool -> Double -> IO ()
run o numThreads False spatDistUnitScaling =
    runCmd o numThreads spatDistUnitScaling
run o numThreads True spatDistUnitScaling = do
    hPutStrLn stderr "Working silently"
    hSilence [stderr] (runCmd o numThreads spatDistUnitScaling)

runCmd :: Subcommand -> Int -> Double -> IO ()
runCmd o numThreads spatDistUnitScaling = case o of
    CmdSerialise opts -> runSerialise opts
    CmdSearch opts    -> runSearch opts numThreads spatDistUnitScaling
    CmdVario opts     -> runVario opts numThreads spatDistUnitScaling
    CmdCross opts     -> runCross opts numThreads spatDistUnitScaling

optParserInfo :: OP.ParserInfo Options
optParserInfo = OP.info (
        OP.helper <*> versionOption <*> (
            Options
        <$> subcommandParser
        <*> optParseQuiet
        <*> optParseSpatDistUnitScaling
        )) (
    OP.briefDesc <>
    OP.progDesc "Spatiotemporal interpolation and search for macroscale archaeological data."
    )

versionOption :: OP.Parser (a -> a)
versionOption = OP.infoOption (showVersion version) (OP.long "version" <> OP.help "Show version")

subcommandParser :: OP.Parser Subcommand
subcommandParser = OP.subparser (
           OP.command "search" searchOptInfo
        <> OP.command "vario" varioOptInfo
        <> OP.command "cross" crossOptInfo
        <> OP.command "serialise" serialiseOptInfo
    )
    where
        searchOptInfo = OP.info (OP.helper <*> (CmdSearch <$> searchOptParser))
            (OP.progDesc "Interpolate dependent variables in space and time (or any independent \
                         \variable space) and optionally determine a probabilistic measure of similarity \
                         \for individual \"search\" observations.")
        varioOptInfo = OP.info (OP.helper <*> (CmdVario <$> varioOptParser))
            (OP.progDesc "Calculate variograms binned based on distances in independent variable space.")
        crossOptInfo = OP.info (OP.helper <*> (CmdCross <$> crossOptParser))
            (OP.progDesc "Compare hyperparameter settings for the interpolation through crossvalidation.")
        serialiseOptInfo = OP.info (OP.helper <*> (CmdSerialise <$> serialiseOptParser))
            (OP.progDesc "Transform input data to compact binary files in .cbor format \
                         \to load it faster in the other subcommands.")

{-# LANGUAGE OverloadedStrings #-}

import           LocEst.CLI.Cross         (CrossOptions (..), runCross)
import           LocEst.CLI.Interface
import           LocEst.CLI.Search        (SearchOptions (..), runSearch)
import           LocEst.CLI.Serialise     (SerialiseOptions (..), runSerialise)
import           LocEst.CLI.Vario         (VarioOptions (..), runVario)
import           LocEst.CLI.VarioFit         (VarioFitOptions (..), runVarioFit)
import           LocEst.Utils

import           Control.Exception        (catch)
import           Data.List                (isInfixOf)
import           Data.Version             (showVersion)
import qualified Options.Applicative      as OP
import           Options.Applicative.Help (pretty)
import           Paths_locest             (version)
import           System.Environment       (getArgs)
import           System.Exit              (exitFailure)
import           System.Info              (arch, compilerName,
                                           fullCompilerVersion, os)
import           System.IO                (hPutStrLn, stderr)
import           System.IO.Silently       (hSilence)

-- data types
data Options = Options {
      _subcommand          :: Subcommand
    , _quiet               :: Bool
    , _spatDistUnitScaling :: Double
    , _configFile          :: Maybe FilePath
    }

data Subcommand =
      CmdSerialise SerialiseOptions
    | CmdSearch SearchOptions
    | CmdVario VarioOptions
    | CmdVarioFit VarioFitOptions
    | CmdCross CrossOptions

-- CLI interface configuration
main :: IO ()
main = do
    hPutStrLn stderr $
        "locest v" ++ showVersion version
         ++ " compiled with " ++ compilerName
         ++ " v" ++ showVersion fullCompilerVersion
    hPutStrLn stderr $
        "running on " ++ os ++ " " ++ arch
    hPutStrLn stderr ""
    -- read command line arguments from cmd and potentially a config file
    rawCmdArgs <- getArgs
    mergedCmdArgs <- case getConfigFilePath rawCmdArgs of
        Nothing -> do
            let cmdArgs = removeConfigFileArg rawCmdArgs
            return cmdArgs
        Just configFilePath -> do
            -- collect arguments
            let cmdArgs = removeConfigFileArg rawCmdArgs
            -- read config file but ignore arguments already on the command line
            -- hacky implementation, but useful for overwriting config files
            configFileArgs <- catch (parseConfigFile cmdArgs configFilePath) handler
            return $ cmdArgs ++ configFileArgs
    -- parse arguments
    (Options subcommand quiet spatDistUnitScaling _) <-
        OP.handleParseResult $
            OP.execParserPure (OP.defaultPrefs {
                  OP.prefShowHelpOnError = False
                , OP.prefShowHelpOnEmpty = True
                , OP.prefColumns = 100
                , OP.prefHelpShowGlobal = False
            }) optParserInfo mergedCmdArgs
    -- run requested subcommand
    catch (run subcommand quiet spatDistUnitScaling) handler
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

run :: Subcommand -> Bool -> Double -> IO ()
run o False spatDistUnitScaling =
    runCmd o spatDistUnitScaling
run o True spatDistUnitScaling = do
    hPutStrLn stderr "Working silently"
    hSilence [stderr] (runCmd o spatDistUnitScaling)

runCmd :: Subcommand -> Double -> IO ()
runCmd o spatDistUnitScaling = case o of
    CmdSerialise opts -> runSerialise opts
    CmdSearch opts    -> runSearch opts spatDistUnitScaling
    CmdVario opts     -> runVario opts spatDistUnitScaling
    CmdVarioFit opts  -> runVarioFit opts
    CmdCross opts     -> runCross opts spatDistUnitScaling

optParserInfo :: OP.ParserInfo Options
optParserInfo = OP.info (
        OP.helper <*> versionOption <*> (
            Options
        <$> subcommandParser
        <*> optParseQuiet
        <*> optParseSpatDistUnitScaling
        <*> optParseConfigFileFake
        )) (
    OP.briefDesc
    <> OP.progDesc "Spatiotemporal interpolation and search for macroscale archaeological data."
    <> OP.footerDoc (
        Just $ pretty $
            "Parallel computing in locest is handled by BLAS, and the number of threads\n"
         ++ "can be set with an environment variable, depening on the BLAS implementation.\n"
         ++ "e.g. OMP_NUM_THREADS = 4 locest search ..."
        )
    )

-- exists only for documentation!
optParseConfigFileFake :: OP.Parser (Maybe FilePath)
optParseConfigFileFake =
  OP.optional $
    OP.strOption
      ( OP.long "configFile"
     <> OP.metavar "FILE"
     <> OP.help "Read additional command line options from FILE, can be overwritten."
      )

versionOption :: OP.Parser (a -> a)
versionOption = OP.infoOption (showVersion version) (OP.long "version" <> OP.help "Show version")

subcommandParser :: OP.Parser Subcommand
subcommandParser = OP.subparser (
           OP.command "search" searchOptInfo
        <> OP.command "vario" varioOptInfo
        <> OP.command "variofit" varioFitOptInfo
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
        varioFitOptInfo = OP.info (OP.helper <*> (CmdVarioFit <$> varioFitOptParser))
            (OP.progDesc "...")
        crossOptInfo = OP.info (OP.helper <*> (CmdCross <$> crossOptParser))
            (OP.progDesc "Compare hyperparameter settings for the interpolation through Monte Carlo \
                         \crossvalidation (repeated random sub-sampling).")
        serialiseOptInfo = OP.info (OP.helper <*> (CmdSerialise <$> serialiseOptParser))
            (OP.progDesc "Transform input data to compact binary files in .cbor format \
                         \to load it faster in the other subcommands.")

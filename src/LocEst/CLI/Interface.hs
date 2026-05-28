{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module LocEst.CLI.Interface where

import           LocEst.CLI.ConfigLang
import           LocEst.CLI.Cross
import           LocEst.CLI.Search
import           LocEst.CLI.Serialise
import           LocEst.CLI.Vario
import           LocEst.CLI.Grid
import           LocEst.Types
import           LocEst.Utils

import           Data.Char                (isSpace, toLower)
import           Data.Functor.Identity    (Identity)
import           Data.List                (groupBy, singleton)
import qualified Data.List.NonEmpty       as N
import           LocEst.CLI.VarioFit      (VarioFitOptions (..))
import qualified Options.Applicative      as OP
import qualified Options.Applicative.Help as OH
import qualified Text.Parsec              as P
import qualified Text.Parsec.String       as P

-- helper functions for optparse applicative help text
s2d :: String -> OH.Doc
s2d str = OH.fillSep $ map OH.pretty $ words str

-- config file that uses the optparse interface

parseConfigFile :: [String] -> FilePath -> IO [String]
parseConfigFile toIgnore configFile = do
    contents <- readFile configFile
    case P.parse parseFile configFile contents of
        Left err -> throwLIO $ show err
        Right x  -> return x
    where
    parseFile :: P.Parser [String]
    parseFile = concat <$> P.many1 (P.try parseEmptyLine P.<|> P.try parseComment P.<|> parseOneArgument)
    parseComment :: P.Parser [String]
    parseComment = do
        _ <- P.manyTill P.space (P.char '#')
        _ <- P.manyTill P.anyChar P.newline
        return []
    parseEmptyLine :: P.Parser [String]
    parseEmptyLine = do
        _ <- P.manyTill P.space P.newline
        return []
    parseOneArgument :: P.Parser [String]
    parseOneArgument = do
        _ <- P.spaces
        argumentName <- P.manyTill (P.noneOf "\n") (P.lookAhead (P.char ':'))
        _ <- P.char ':'
        _ <- P.spaces
        argumentValue <- P.manyTill ((P.try parseComment >> return ' ') P.<|> P.anyChar) (P.lookAhead (P.char ';'))
        _ <- P.char ';'
        _ <- P.try parseComment P.<|> parseEmptyLine
        if dash argumentName `elem` toIgnore
        then pure []
        else do
            if map toLower argumentValue == "true"
            then return [dash argumentName]
            else return [dash argumentName, trim argumentValue]
    dash :: String -> String
    dash s
      | length s == 1 = '-'  :  s
      | otherwise     = "--" ++ s
    trim :: String -> String
    trim = let f = reverse . dropWhile isSpace in f . f

-- optparse-applicative interface

gridOptParser :: OP.Parser GridOptions
gridOptParser = GridOptions <$> optParseInPolygonFile
                            <*> optParseInResolutionX
                            <*> optParseInResolutionY
                            <*> optParseOutFile

optParseInPolygonFile :: OP.Parser FilePath
optParseInPolygonFile = OP.strOption (
    OP.long "polygonFile" <>
    OP.metavar "FILE" <>
    OP.help "...")
    
optParseInResolutionX :: OP.Parser Double
optParseInResolutionX = OP.option OP.auto (
    OP.long "x" <>
    OP.help "..." <>
    OP.metavar "DOUBLE"
    )

optParseInResolutionY :: OP.Parser Double
optParseInResolutionY = OP.option OP.auto (
    OP.long "y" <>
    OP.help "..." <>
    OP.metavar "DOUBLE"
    )

serialiseOptParser :: OP.Parser SerialiseOptions
serialiseOptParser = SerialiseOptions <$> OP.subparser (
                        OP.command "obs" (OP.info (OP.helper <*> (
                            SerialiseObsFile
                            <$> optParseInObservationFile
                            )) (OP.progDesc "Serialise --obsFile."))
                     <> OP.command "grid" (OP.info (OP.helper <*> (
                            SerialiseGridFile
                            <$> optParseInIndepVarGridFile
                            )) (OP.progDesc "Serialise --gridFile."))
                     <> OP.command "tempsamp" (OP.info (OP.helper <*> (
                            SerialiseObsTempSamplesFile
                            <$> optParseInObservationFile
                            <*> optParseInObsTempSamplesFile
                            )) (OP.progDesc "Serialise --tempSampFile."))
                     <> OP.command "selfdist" (OP.info (OP.helper <*> (
                            SerialiseSelfDistMatrixPerIndepVar
                            <$> (VecFileObs <$> optParseInObservationFile OP.<|> VecFileGrid <$> optParseInIndepVarGridFile)
                            <*> optParseInObsObsDistFile --OP.<|> optParseInGridGridDistFile
                            )) (OP.progDesc "Serialise self distance matrix."))
                     <> OP.command "crossdist" (OP.info (OP.helper <*> (
                            SerialiseCrossDistMatrixPerIndepVar
                            <$> optParseInObservationFile
                            <*> optParseInIndepVarGridFile
                            <*> optParseInObsGridDistFile
                            )) (OP.progDesc "Serialise cross distance matrix."))
                     ) <*> optParseOutFileCbor

searchOptParser :: OP.Parser SearchOptions
searchOptParser = SearchOptions
                        <$> optParseInObservationFile
                        <*> OP.optional optParseInObsTempSamplesFile
                        <*> optParseInIndepVarGridFile
                        <*> OP.optional optParseTempGridString
                        <*> OP.optional optParseSearchPositions
                        <*> optParseKernDefString
                        <*> OP.optional optParseInObsObsDistFile
                        <*> OP.optional optParseInObsGridDistFile
                        -- <*> OP.optional optParseInGridGridDistFile
                        <*> optParseTopNObs
                        <*> optParseOutFile

varioOptParser :: OP.Parser VarioOptions
varioOptParser = VarioOptions
                        <$> optParseInObservationFile
                        <*> OP.optional optParseInObsObsDistFile
                        <*> optParseAcrossSettings
                        <*> optParseSpaceTimeScaling
                        <*> optParseIndepVarsThresholds
                        <*> optParseIndepVarsCrossThresholds
                        <*> optParseSubsamplingIterations
                        <*> optParseSubsamplingFraction
                        <*> optParseSeed
                        <*> optParseOutFile
                        <*> optParseVarioOutMode

varioFitOptParser :: OP.Parser VarioFitOptions
varioFitOptParser = VarioFitOptions
                        <$> optParseEmpiricalVarioFile
                        <*> optParseKernelShapes
                        <*> optParseFreeSill
                        <*> optParseOutFile

crossOptParser :: OP.Parser CrossOptions
crossOptParser = CrossOptions
                        <$> optParseInObservationFile
                        <*> optParseKernDefStringPermutations
                        <*> optParseTestTrainingFraction
                        <*> optParseCrossvalIterations
                        <*> optParseSeed
                        <*> OP.optional optParseInObsObsDistFile
                        <*> optParseOutFile


optParseEmpiricalVarioFile :: OP.Parser FilePath
optParseEmpiricalVarioFile = OP.strOption (
    OP.long "empVarioFile" <>
    OP.metavar "FILE" <>
    OP.help "Path to a .tsv file as generated by varioemp.")

optParseInObsObsDistFile :: OP.Parser FilePath
optParseInObsObsDistFile = OP.strOption (
       OP.long    "obsObsDistFile"
    <> OP.metavar "FILE"
    <> OP.helpDoc ( Just (
                      s2d "Path to a .tsv/.cbor file with distances between pairs of observations \
                          \along arbitrary independent variables. \
                          \With this the given distances will not be calculated from the respective \
                          \coordinates, but looked up in this table. \
                          \The pairs must be ordered first by id1 and then within \
                          \that by id2 (both as in --obsFile). Every pair must only be given once, \
                          \as the distances are symmetric. \
                          \The ID columns can be omitted - they are neither read nor validated."
    <> OH.hardline <>     "┌───┬───┬─────┬────┬───────┬───────┐"
    <> OH.hardline <>     "│id1│id2│space│time│indepV1│indepV2│ > [id1] (optional):"
    <> OH.hardline <>     "├───┼───┼─────┼────┼───────┼───────┤   Observations identifier 1"
    <> OH.hardline <>     "│ a │ a │     │    │       │       │ > [id2] (optional):"
    <> OH.hardline <>     "│ b │ a │     │    │       │       │   Observations identifier 2"
    <> OH.hardline <>     "│ b │ b │     │    │       │       │ > [space]/[time]/[indepV*]:"
    <> OH.hardline <>     "│ c │ a │     │    │       │       │   Distances along dimensions"
    <> OH.hardline <>     "└───┴───┴─────┴────┴───────┴───────┘"
    ))
    )

optParseInObsGridDistFile :: OP.Parser FilePath
optParseInObsGridDistFile = OP.strOption (
       OP.long    "obsGridDistFile"
    <> OP.metavar "FILE"
    <> OP.helpDoc ( Just (
                      s2d "Path to a .tsv/.cbor file with distances between pairs of observations and \
                          \prediction grid positions along arbitrary independent variables. \
                          \The pairs must be ordered first by gridID (as in --gridFile) and then within \
                          \that by obsID (as in --obsFile)."
    <> OH.hardline <>     "┌──────┬─────┬─────┬────┬───────┬───────┐"
    <> OH.hardline <>     "│gridID│obsID│space│time│indepV1│indepV2│ > [gridID] (optional):"
    <> OH.hardline <>     "├──────┼─────┼─────┼────┼───────┼───────┤   Grid position identifier"
    <> OH.hardline <>     "│   x  │  a  │     │    │       │       │ > [obsID] (optional):"
    <> OH.hardline <>     "│   x  │  b  │     │    │       │       │   Observations identifier"
    <> OH.hardline <>     "│   y  │  a  │     │    │       │       │ > [space]/[time]/[indepV*]:"
    <> OH.hardline <>     "│   y  │  b  │     │    │       │       │   Distances along dimensions"
    <> OH.hardline <>     "└──────┴─────┴─────┴────┴───────┴───────┘"
    ))
    )

optParseInGridGridDistFile :: OP.Parser FilePath
optParseInGridGridDistFile = OP.strOption (
       OP.long    "gridGridDistFile"
    <> OP.metavar "FILE"
    <> OP.helpDoc ( Just (
                      s2d "Path to a .tsv/.cbor file with distances between pairs of prediction grid \
                          \positions along arbitrary independent variables. \
                          \With this the given distances will not be calculated from the respective \
                          \coordinates, but looked up in this table. \
                          \The pairs must be ordered first by id1 (as in --gridFile) and then within \
                          \that by id2 (also as in --gridFile). Every pair must only be given once, \
                          \as the distances are symmetric. \
                          \The ID columns can be omitted - they are not read or validated."
    <> OH.hardline <>     "┌───┬───┬─────┬────┬───────┬───────┐"
    <> OH.hardline <>     "│id1│id2│space│time│indepV1│indepV2│ > [id1] (optional):"
    <> OH.hardline <>     "├───┼───┼─────┼────┼───────┼───────┤   Grid position identifier"
    <> OH.hardline <>     "│ a │ a │     │    │       │       │ > [id2] (optional):"
    <> OH.hardline <>     "│ b │ a │     │    │       │       │   Grid position identifier"
    <> OH.hardline <>     "│ b │ b │     │    │       │       │ > [space]/[time]/[indepV*]:"
    <> OH.hardline <>     "│ c │ a │     │    │       │       │   Distances"
    <> OH.hardline <>     "└───┴───┴─────┴────┴───────┴───────┘"
    ))
    )

optParseIndepVarsThresholds :: OP.Parser IndepVarsThresholds
optParseIndepVarsThresholds = OP.option (OP.eitherReader readIndepVarsThresholds) (
       OP.long "indepVarsThresholds"
    <> OP.metavar "\"c(space=DOUBLE,time=DOUBLE,indepV1=DOUBLE,...)\""
    <> OP.value (makeValuesPerIndepVar [])
    <> OP.helpDoc ( Just (
                      s2d "Thresholds for filtering distances in the variogram compution. \
                          \Does not work if distances are aggregated across independent variables \
                          \with --across."
    ))
    )

optParseIndepVarsCrossThresholds :: OP.Parser IndepVarsThresholds
optParseIndepVarsCrossThresholds = OP.option (OP.eitherReader readIndepVarsThresholds) (
       OP.long "indepVarsCrossThresholds"
    <> OP.metavar "\"c(space=DOUBLE,time=DOUBLE,indepV1=DOUBLE,...)\""
    <> OP.value (makeValuesPerIndepVar [])
    <> OP.helpDoc ( Just (
                      s2d "Thresholds for filtering distances across independent variables. \
                          \When computing a variogram for temporal distances, for example, it might \
                          \be desirable to constrain the spatial distances, so that only observations \
                          \in spatial proximity are considered. So this threshold does not \
                          \filter on the focal independent variable, but on the other independent variables. \
                          \Does not work if distances are aggregated across independent variables \
                          \with --across."
    ))
    )

readIndepVarsThresholds :: String -> Either String IndepVarsThresholds
readIndepVarsThresholds s =
    case P.runParser parseIndepVarsThresholds () "" s of
        Left err -> Left $ showParsecErr err
        Right x  -> Right x
    where
        parseIndepVarsThresholds = do
            res <- parseNamedVector parseIndepVarName parsePositiveDouble
            return (makeValuesPerIndepVar res)

readSpaceTime :: String -> Either String (Double, Double)
readSpaceTime s =
    case P.runParser parseSpaceTime () "" s of
        Left err -> Left $ showParsecErr err
        Right x  -> Right x
parseSpaceTime :: P.Parser (Double, Double)
parseSpaceTime = do
    parseRecordType "c" $ do
        a <- parseArgument "space" parseDouble
        b <- parseArgument "time" parseDouble
        return (a, b)

optParseVarioOutMode :: OP.Parser BinModeSettings
optParseVarioOutMode = OP.option (OP.eitherReader readOutMode) (
    OP.long "outMode" <>
    OP.metavar "\"equalSize(n=INT)|oneBinMax(max = c(indepV1=DOUBLE,indepV2=DOUBLE,...)\"" <>
    OP.value (BinByNrBins 100)
    <> OP.helpDoc ( Just (
                      s2d "The binning procedure that should be applied for the variogram."
    <> OH.hardline <> s2d "equalSize(n): Bins the observations into n bins with an equal amount of \
                          \observations."
    <> OH.hardline <> s2d "oneBinMax(max = c(indepV1=DOUBLE,indepV2=DOUBLE,...): Only create one bin \
                          \per independent and dependent variable with a given upper limit."
    <> OH.hardline <> s2d "The output of vario depends on the binning, but generally \
                          \it returns a table like this:"
    <> OH.hardline <>     "┌─────────┬────────┬──────┬───────┬───────┬───────┬────────┬────────┐"
    <> OH.hardline <>     "│iteration│indepVar│depVar│bin_min│bin_mid│bin_max│variance│nr_pairs│"
    <> OH.hardline <>     "├─────────┼────────┼──────┼───────┼───────┼───────┼────────┼────────┤"
    <> OH.hardline <>     "│         │        │      │       │       │       │        │        │"
    <> OH.hardline <>     "└─────────┴────────┴──────┴───────┴───────┴───────┴────────┴────────┘"
    <> OH.hardline <>     "> [indepVar]: Independent variable"
    <> OH.hardline <>     "> [depVar]: Dependent variable"
    <> OH.hardline <> s2d "> [bin_min,bin_mid,bin_max]: Start, center and end point of each \
                             \independent variable bin"
    <> OH.hardline <> s2d "> [variance]: Variance calculated for the dependent variable \
                             \based on all pairs of observations in the respective bin"
    ))
    )
    where
        readOutMode :: String -> Either String BinModeSettings
        readOutMode s =
            case P.runParser parseBinMode () "" s of
                Left err -> Left $ showParsecErr err
                Right x  -> Right x
        parseBinMode = P.try parseEqualSize P.<|> parseOneBinMax
        parseEqualSize = do
            res <- parseRecordType "equalSize" $ do
                n <- parseArgument "n" parseInt
                return n
            return (BinByNrBins res)
        parseOneBinMax = do
            res <- parseRecordType "oneBinMax" $ do
                maxPerIndepVar <- parseArgument "max" (parseNamedVector parseIndepVarName parseDouble)
                return $ makeValuesPerIndepVar maxPerIndepVar
            return (BinForNugget res)

optParseSeed :: OP.Parser (Maybe Int)
optParseSeed = OP.option (Just <$> OP.auto) (
       OP.long  "seed"
    <> OP.metavar "INT"
    <> OP.value Nothing
    <> OP.helpDoc ( Just (
                      s2d "Seed for the random number generator used to create data subsets. \
                          \Default: A random seed (not reproducible)."
    ))
    )

optParseTestTrainingFraction :: OP.Parser Double
optParseTestTrainingFraction = OP.option (OP.eitherReader readFraction) (
       OP.long    "testFraction"
    <> OP.metavar "DOUBLE"
    <> OP.value 0.2
    <> OP.helpDoc ( Just (
                          s2d "Fraction of the observations that should be used as test data for the \
                              \crossvalidation. 1 - testFraction will be used as training data. \
                              \The fraction must be between 0 and 1. \
                              \When the fraction is so large that the number of test observations \
                              \equals the total number of observations, then all observations are used \
                              \for the training set. In this special case the seed has no effect and all \
                              \iterations yield the same result. Default: 0.2"
    ))
    )
    where
        readFraction :: String -> Either String Double
        readFraction s =
            case P.runParser parseFraction () "" s of
                Left err -> Left $ showParsecErr err
                Right x  -> Right x

optParseSubsamplingFraction :: OP.Parser Double
optParseSubsamplingFraction = OP.option (OP.eitherReader readFraction) (
       OP.long    "omitFraction"
    <> OP.metavar "DOUBLE"
    <> OP.value 0.2
    <> OP.helpDoc ( Just (
                          s2d "Fraction of the observations that should be randomly omitted in every \
                              \subsampling run. Default: 0.2"
    ))
    )
    where
        readFraction :: String -> Either String Double
        readFraction s =
            case P.runParser parseFraction () "" s of
                Left err -> Left $ showParsecErr err
                Right x  -> Right x

optParseSubsamplingIterations :: OP.Parser Int
optParseSubsamplingIterations = OP.option OP.auto (
       OP.long    "iterations"
    <> OP.metavar "INT"
    <> OP.value 0
    <> OP.helpDoc ( Just (
                      s2d "Number subsampling iterations. How often should the input observations \
                          \be subset to compute the variogram. No subsetting is done if this is set \
                          \to 0. Default: 0"
    ))
    )

optParseCrossvalIterations :: OP.Parser Int
optParseCrossvalIterations = OP.option OP.auto (
       OP.long    "iterations"
    <> OP.metavar "INT"
    <> OP.value 100
    <> OP.helpDoc ( Just (
                      s2d "Number of crossvalidation iterations. How often should the input observations \
                          \be reshuffled and split into test and training data for each kernel parameter \
                          \setting. Default: 100"
    ))
    )

optParseTopNObs :: OP.Parser Int
optParseTopNObs = OP.option OP.auto (
       OP.long    "topObs"
    <> OP.metavar "INT"
    <> OP.value 0
    <> OP.helpDoc ( Just (
                      s2d "Number of most informative observations that should be documented. \
                          \When this is > 0, then a list of n observations with the highest weight \
                          \is computed for each prediction grid point and dependent variable. \
                          \It is written to an output column [grid_top_obs_<dependent_variable>]. \
                          \Default: 0"
    ))
    )

optParseAcrossSettings :: OP.Parser AcrossSettings
optParseAcrossSettings = OP.option (OP.eitherReader readAcrossSettings) (
    OP.long "across" <>
    OP.metavar "IndepVars|DepVars|Both|AllCombinations" <>
    OP.value AcrossNone
    <> OP.helpDoc ( Just (
                      s2d "Variogram distance aggregation modes. If this is set, then individual \
                          \independent or dependent dimensions will be merged for the variogram \
                          \calculation. That means the variogram will then be computed on \
                          \Euclidean distances in the shared n-dimensional space of the aggregated \
                          \dimensions. --spaceTimeScaling allows to scale space and time relative \
                          \to each other when --across is set to merge independent variable \
                          \dimensions."
    ))
    )
    where
        readAcrossSettings :: String -> Either String AcrossSettings
        readAcrossSettings s =
            case P.runParser parseAcrossSettings () "" s of
                Left err -> Left $ showParsecErr err
                Right x  -> Right x
        parseAcrossSettings  = P.try parseAcrossIndepVars P.<|> P.try parseAcrossDepVars P.<|> P.try parseAcrossBoth P.<|> parseAcrossComb
        parseAcrossIndepVars = P.string "IndepVars"       >> return AcrossIndepVars
        parseAcrossDepVars   = P.string "DepVars"         >> return AcrossDepVars
        parseAcrossBoth      = P.string "Both"            >> return AcrossBoth
        parseAcrossComb      = P.string "AllCombinations" >> return AcrossComb

optParseSpaceTimeScaling :: OP.Parser (Double,Double)
optParseSpaceTimeScaling = OP.option (OP.eitherReader readSpaceTime) (
       OP.long "spaceTimeScaling"
    <> OP.metavar "DOUBLE"
    <> OP.metavar "\"c(space = DOUBLE, time = DOUBLE)\""
    <> OP.helpDoc ( Just (
                      s2d "Space-time scaling factors. All temporal and spatial distances will be multiplied by \
                          \the respective factors before combining the distances as one Euclidean distance. \
                          \Only relevant with --across. Default: c(space = 1, time = 1)."
    ))
    <> OP.value (1,1)
    )

optParseQuiet :: OP.Parser Bool
optParseQuiet = OP.switch (
    OP.long "quiet" <>
    OP.short 'q' <>
    OP.help "Suppress the printing of progress messages to the stderr stream on the command line."
    )

optParseFreeSill :: OP.Parser Bool
optParseFreeSill = OP.switch (
    OP.long "freeSill" <>
    OP.help "Should the sill be a free parameter that is optimized in the variogram fitting process? \
            \By default it is fixed to the total variance (read from the \"infinite\" bin in the \
            \varioemp output) and does not get fitted."
    )

optParseSpatDistUnitScaling :: OP.Parser Double
optParseSpatDistUnitScaling = OP.option OP.auto (
    OP.long "spaceScaling" <>
    OP.help "Spatial distances computed from input coordinates \
            \or read from input tables are rescaled by this factor. By default set to 0.001 for \
            \input data in metres and output in kilometres." <>
    OP.metavar "DOUBLE" <>
    OP.value 0.001
    )

optParseInObservationFile :: OP.Parser FilePath
optParseInObservationFile = OP.strOption (
       OP.long    "obsFile"
    <> OP.short   'i'
    <> OP.metavar "FILE"
    <> OP.helpDoc ( Just (
                          s2d "Path to a .tsv/.cbor file with the input observations to inform \
                              \the interpolation. Columns for the basic spatiotemporal case with \
                              \two dependent variables:"
    <> OH.hardline <>    "┌─────┬───┬───┬────────┬─────┬─────┬───┐"
    <> OH.hardline <>    "│obsID│ x │ y │yearBCAD│depV1│depV2│...│"
    <> OH.hardline <>    "├─────┼───┼───┼────────┼─────┼─────┼───┤"
    <> OH.hardline <>    "│     │   │   │        │     │     │   │"
    <> OH.hardline <>    "└─────┴───┴───┴────────┴─────┴─────┴───┘"
    <> OH.hardline <> s2d "> [obsID]: Observation identifier"
    <> OH.hardline <> s2d "> [x, y, yearBCAD] or [longitude, latitude, yearBCAD] or \
                          \[indepV1, indepV2, ...]: Independent variable position where the \
                          \first two options belong to the spatiotemporal interpolation setup, \
                          \and the last to the arbitrary dimension setup. There all \
                          \variables require the prefix \"indep\" followed by any variable name, \
                          \e.g. \"V1\" and \"V2\"."
    <> OH.hardline <> s2d "> [depV1, depV2, ...]: Dependent variable position. All variables require \
                          \the prefix \"dep\" followed by any variable name, e.g. \"V1\" and \"V2\"."
    <> OH.hardline <> s2d "> [...]: Additional variables are carried along."
    ))
    )

optParseInObsTempSamplesFile :: OP.Parser FilePath
optParseInObsTempSamplesFile = OP.strOption (
    OP.long "tempSampFile" <>
    OP.metavar "FILE" <>
    OP.helpDoc ( Just (
                      s2d "Path to a .tsv/.cbor file with random age permutations per observation. \
                          \If this is given, then the temporal position of each sample is not read from \
                          \--obsFile, but looked up in this table. The observations must be ordered like \
                          \--obsFile and include the same amount of age samples per observation."
    <> OH.hardline <>     "┌─────┬────────┐"
    <> OH.hardline <>     "│obsID│yearBCAD│ > [obsID]:"
    <> OH.hardline <>     "├─────┼────────┤   Observations identifier"
    <> OH.hardline <>     "│   a │        │ > [yearBCAD]"
    <> OH.hardline <>     "│   a │        │   Age sample"
    <> OH.hardline <>     "│   b │        │"
    <> OH.hardline <>     "│   b │        │"
    <> OH.hardline <>     "└─────┴────────┘"
    ))
    )

optParseSearchPositions :: OP.Parser DepVarsPredGridSettings
optParseSearchPositions =
           DirectDepVarsGridSettings <$> optParseSearchDepVarsPos
    OP.<|> SearchObsDepVarsGridSettings <$> optParseInSearchObservationFile

optParseInIndepVarGridFile :: OP.Parser FilePath
optParseInIndepVarGridFile = OP.strOption (
       OP.long    "gridFile"
    <> OP.short   'g'
    <> OP.metavar "FILE"
    <> OP.helpDoc ( Just (
                      s2d "Path to a .tsv/.cbor file with independent variable positions \
                          \(e.g. spatiotemporal coordinates) where interpolation and search should be performed."
    <> OH.hardline <>     "┌───┬───┬──────────┐"
    <> OH.hardline <>     "│ x │ y │ yearBCAD │ > [x, y] or [longitude, latitude]"
    <> OH.hardline <>     "├───┼───┼──────────┤   Spatial position"
    <> OH.hardline <>     "│   │   │          │ > [yearBCAD]"
    <> OH.hardline <>     "└───┴───┴──────────┘   Temporal position"
    <> OH.hardline <>     "OR"
    <> OH.hardline <>     "┌───────┬───────┬───┐"
    <> OH.hardline <>     "│indepV1│indepV2│...│ > [indepV1, ...]:"
    <> OH.hardline <>     "├───────┼───────┼───┤   Independent variable"
    <> OH.hardline <>     "│       │       │   │   positions"
    <> OH.hardline <>     "└───────┴───────┴───┘"
    <> OH.hardline <> s2d "[yearBCAD] can be overwritten by --tempGrid in locest search."
    ))
    )

optParseTempGridString :: OP.Parser [AbsRelTempPos]
optParseTempGridString = OP.option (OP.eitherReader readTempGridString) (
       OP.long    "tempGrid"
    <> OP.short   't'
    <> OP.metavar "\"absolute|relative(years = YEAR|c(YEAR1,YEAR2,...)|START:STOP:BY)\""
    <> OP.helpDoc ( Just (
                      s2d "Temporal positions in years BC/AD where interpolation and search should \
                          \be performed. absolute(...) means absolute years BC/AD. relative(...) only \
                          \works with --searchObsFile and means before or after the age of the respective \
                          \search samples. Negative integer numbers mark years BC or before the search sample, \
                          \positive numbers years AD or after the search sample. \
                          \A list of years can be defined in three ways:"
    <> OH.hardline <> s2d "> YEAR: One year, e.g. \"-3000\" for 3000BC"
    <> OH.hardline <> s2d "> c(YEAR1,YEAR2,...): A list of years, e.g. \"c(-3000, 1000)\" for 3000BC \
                          \and 1000AD"
    <> OH.hardline <> s2d "> START:STOP:BY: A sequence  of years, e.g. \"-3000:-2000:500\" for 3000BC, \
                          \2500BC and 2000BC"
    ))
    )
    where
        readTempGridString :: String -> Either String [AbsRelTempPos]
        readTempGridString s =
            case P.runParser parseAbsRelString () "" s of
                Left err -> Left $ showParsecErr err
                Right x  -> Right x
        parseAbsRelString :: P.Parser [AbsRelTempPos]
        parseAbsRelString = do
            P.try parseAbs P.<|> parseRel
        parseAbs :: P.Parser [AbsRelTempPos]
        parseAbs = do
            res <- parseRecordType "absolute" $ do
                y <- parseArgument "years" parseTempGridString
                return y
            return $ map AbsTempPos res
        parseRel = do
            res <- parseRecordType "relative" $ do
                y <- parseArgument "years" parseTempGridString
                return y
            return $ map RelTempPos res
        parseTempGridString :: P.Parser [Int]
        parseTempGridString = do
            P.try parseYearSequence P.<|> P.try parseYearList P.<|> parseSingleYear
            where
                parseYearSequence = parseIntSequence
                parseYearList = parseVector parseInt
                parseSingleYear = singleton <$> parseInt

optParseSearchDepVarsPos :: OP.Parser [DepVarsPos]
optParseSearchDepVarsPos = OP.option (OP.eitherReader readSearchDepVarsPos) (
       OP.long    "searchDepVarsPos"
    <> OP.short   'd'
    <> OP.metavar "\"c(depV1=DOUBLE,depV1=c(DOUBLE,DOUBLE,...),depV3=START:STOP:BY,...)\""
    <> OP.helpDoc ( Just (
                      s2d "Dependent variable positions that should be \"searched\" for, so for which \
                          \similarity probabilities in the interpolated field should be computed. \
                          \Each dependent variable must be specified in a named list \"c(depV1 = ..., depV2 = ...)\". \
                          \And for each one either a single coordinate, a list of coordinates, \
                          \or a sequence of coordinates can be specified."
    ))
    )
    where
        readSearchDepVarsPos :: String -> Either String [DepVarsPos]
        readSearchDepVarsPos s =
            case P.runParser parseSearchDepVarsPos () "" s of
                Left err -> Left $ showParsecErr err
                Right x  -> Right x
        parseSearchDepVarsPos :: P.Parser [DepVarsPos]
        parseSearchDepVarsPos = do
            res <- parseNamedVector parseDepVarName (P.try parseSequence P.<|> P.try parseList P.<|> parseSingle)
            let flattened = concatMap (\(str, dblList) -> map (\dbl -> (str, dbl)) dblList) res
                grouped = groupBy (\(str1, _) (str2, _) -> str1 == str2) flattened
                permutations = sequenceA grouped
            return $ map makeValuesPerDepVar permutations
            where
                parseSequence = parseDoubleSequence
                parseList = parseVector parseDouble
                parseSingle = singleton <$> parseDouble

optParseInSearchObservationFile :: OP.Parser FilePath
optParseInSearchObservationFile = OP.strOption (
       OP.long    "searchObsFile"
    <> OP.short   's'
    <> OP.metavar "FILE"
    <> OP.helpDoc ( Just (
                      s2d "Path to a .tsv/.cbor file with input observations whose dependent variable \
                          \positions should be \"searched\" for, so for which similarity probabilities \
                          \in the interpolated field should be computed. Structured as --obsFile."
    ))
    )

optParseOutFileCbor :: OP.Parser FilePath
optParseOutFileCbor = OP.strOption (
       OP.long  "outFile"
    <> OP.short 'o'
    <> OP.metavar "FILE"
    <> OP.helpDoc ( Just (
                      s2d "Path to an output .cbor file."
    ))
    )

optParseOutFile :: OP.Parser (Maybe FilePath)
optParseOutFile = OP.option (Just <$> OP.str) (
       OP.long  "outFile"
    <> OP.short 'o'
    <> OP.metavar "FILE"
    <> OP.value Nothing
    <> OP.helpDoc ( Just (
                      s2d "Path to an output .tsv file. If not provided, then the output will be written \
                          \to stdout."
    ))
    )

optParseKernDefString :: OP.Parser KernelDefinition
optParseKernDefString = OP.option (OP.eitherReader readKernDefString) (
       OP.long    "algodef"
    <> OP.short   'a'
    <> OP.metavar "DSL"
    <> OP.helpDoc ( Just (
                      s2d "Algorithm parameter settings for the interpolation."
    <> OH.hardline <>     "┌────────────────────┐"
    <> OH.hardline <>     "│def(                │"
    <> OH.hardline <>     "│  algorithm = GPR,  │ > interpolation algorithm"
    <> OH.hardline <>     "│                    │   - either GPR or KAS"
    <> OH.hardline <>     "│  depVars = c(      │ > named list of dependent variables"
    <> OH.hardline <>     "│    depV1 = k(      │   - first dependent variable"
    <> OH.hardline <>     "│      shape = SqEx, │     - either SqEx = Squared exponential"
    <> OH.hardline <>     "│                    │           or Ex = Exponential"
    <> OH.hardline <>     "│                    │           or Linear = Linear kernel"
    <> OH.hardline <>     "│      lengths = c(  │     - named list with length scale"
    <> OH.hardline <>     "│        space = ... │       for each independent variable"
    <> OH.hardline <>     "│        time = ...  │       (can also be \"indep...\")"
    <> OH.hardline <>     "│      ),            │"
    <> OH.hardline <>     "│      nugget = ...  │     - (optional) nugget parameter"
    <> OH.hardline <>     "│                    │       only relevant for GPR"
    <> OH.hardline <>     "│    ),              │"
    <> OH.hardline <>     "│    depV2 = k(...)  │   - second dependent variable"
    <> OH.hardline <>     "│  )                 │"
    <> OH.hardline <>     "│)                   │"
    <> OH.hardline <>     "└────────────────────┘"
    <> OH.hardline <> s2d "Any number of dependent and independent variables can be specified, but \
                          \all given variables must exist in --obsFile and --gridFile."
    ))
    )
    where
        readKernDefString :: String -> Either String KernelDefinition
        readKernDefString s =
            case P.runParser parseAKernDefString () "" s of
                Left err -> Left $ showParsecErr err
                Right x  -> Right x
        parseAKernDefString :: P.Parser KernelDefinition
        parseAKernDefString = do
                    kerndef <- parseRecordType "def" $ do
                        algo <- parseArgument "algorithm" parseAlgorithm
                        kernelSets <- parseArgument "depVars" (parseNamedVector parseDepVarName parseShapeLengths)
                        return (algo, kernelSets)
                    return $ makeKernelDefinition (fst kerndef) $
                        map (\(name,(s,l,n)) -> KernelOneDepVar name s l n) (snd kerndef)
        parseShapeLengths = do
            parseRecordType "k" $ do
                s <- parseArgument "shape" parseKernelShapes
                l <- parseArgument "lengths" parseKernelLengths
                n <- parseArgumentOptional "nugget" parseNugget
                return (s,l,n)
        parseAlgorithm = do
            algo <- parseAnyString
            makeAlgorithm algo
        parseKernelLengths = KernelLengths . makeValuesPerIndepVar <$> parseNamedVector parseIndepVarName parseDouble
        parseNugget = parsePositiveFloatNumber

optParseKernelShapes :: OP.Parser [KernelShape]
optParseKernelShapes = OP.many $ OP.option (OP.eitherReader readKernelShapeString) (
       OP.long    "kernel"
    <> OP.short   'k'
    <> OP.metavar "SqEx|Ex|Linear"
    <> OP.helpDoc ( Just (
                      s2d "Kernel shapes that can be fitted. Can be given multiple times to fit \
                          \multiple kernels."
    ))
    )
    where
        readKernelShapeString :: String -> Either String KernelShape
        readKernelShapeString s =
            case P.runParser parseKernelShapes () "" s of
                Left err -> Left $ showParsecErr err
                Right x  -> Right x

parseKernelShapes :: P.ParsecT String () Identity KernelShape
parseKernelShapes = do
    shape <- parseAnyString
    makeKernelShape shape

optParseKernDefStringPermutations :: OP.Parser (N.NonEmpty KernelDefinition)
optParseKernDefStringPermutations = OP.option (OP.eitherReader readKernDefString) (
       OP.long    "algodef"
    <> OP.short   'a'
    <> OP.metavar "DSL"
    <> OP.helpDoc ( Just (
                      s2d "Algorithm parameter settings for the crossvalidation."
    <> OH.hardline <>     "┌────────────────────┐"
    <> OH.hardline <>     "│def(                │"
    <> OH.hardline <>     "│  algorithm = GPR,  │ > interpolation algorithm"
    <> OH.hardline <>     "│                    │   - either GPR or KAS"
    <> OH.hardline <>     "│  depVars = c(      │ > named list of dependent variables"
    <> OH.hardline <>     "│    depV1 = k(      │   - first dependent variable"
    <> OH.hardline <>     "│      shape = SqEx, │     - either SqEx = Squared exponential"
    <> OH.hardline <>     "│                    │           or Ex = Exponential"
    <> OH.hardline <>     "│                    │           or Linear = Linear kernel"
    <> OH.hardline <>     "│      lengths = c(  │     - named list with length scale"
    <> OH.hardline <>     "│        space = ... │       for each independent variable *"
    <> OH.hardline <>     "│        time = ...  │       (can also be \"indep...\")"
    <> OH.hardline <>     "│      ),            │"
    <> OH.hardline <>     "│      nugget = ...  │     - (optional) nugget parameter"
    <> OH.hardline <>     "│                    │       only relevant for GPR"
    <> OH.hardline <>     "│    ),              │"
    <> OH.hardline <>     "│    depV2 = k(...)  │   - second dependent variable"
    <> OH.hardline <>     "│  )                 │"
    <> OH.hardline <>     "│)                   │"
    <> OH.hardline <>     "└────────────────────┘"
    <> OH.hardline <> s2d "Any number of dependent and independent variables can be specified, but \
                          \all variables must also exist in --obsFile and --gridFile."
    <> OH.hardline <> s2d "* Unlike for the search subcommand, here multiple values can be given for the \
                          \independent variables' length scale parameters. They can be provided \
                          \either as a list with \"c(100,200,...)\" or as a sequence using the \
                          \START:STOP:BY syntax, e.g. \"100:1000:100\". The crossvalidation will try \
                          \all permutations of the lengthscale settings."
    ))
    )
    where
        readKernDefString :: String -> Either String (N.NonEmpty KernelDefinition)
        readKernDefString s =
            case P.runParser parseAKernDefString () "" s of
                Left err -> Left $ showParsecErr err
                Right xs -> case N.nonEmpty xs of
                    Just ne -> Right ne
                    Nothing -> Left "No kernel definitions found (expected at least one)"
        parseAKernDefString :: P.Parser [KernelDefinition]
        parseAKernDefString = do
            kerndefs <- parseRecordType "def" $ do
                algo <- parseArgument "algorithm" parseAlgorithm
                kernelSets <- parseArgument "depVars" (parseNamedVector parseDepVarName parseShapeLengths)
                return (algo, kernelSets)
            -- all depVar permutations
            -- let (algo, depVars) = kerndefs
            --     expandedPerDepVar :: [[KernelOneDepVar]]
            --     expandedPerDepVar =
            --         [ [ KernelOneDepVar name shape lengths nugget
            --           | lengths <- lengthsList
            --           ]
            --         | (name, (shape, lengthsList, nugget)) <- depVars
            --         ]
            --     allCombinations :: [[KernelOneDepVar]]
            --     allCombinations = sequence expandedPerDepVar
            -- return $ map (makeKernelDefinition algo) allCombinations
            -- one KernelDefinition per depvar, sweeping only its permutations
            let (algo, depVars) = kerndefs
                allVariants :: [[KernelOneDepVar]]
                allVariants =
                  concat
                    [ [ [KernelOneDepVar name shape lengths nugget]
                      | lengths <- lengthsList
                      ]
                    | (name, (shape, lengthsList, nugget)) <- depVars
                    ]
            return $ map (makeKernelDefinition algo) allVariants
        parseShapeLengths = do
            parseRecordType "k" $ do
                s <- parseArgument "shape" parseKernelShapes
                ls <- parseArgument "lengths" parseKernelLengths
                n <- parseArgumentOptional "nugget" parseNugget
                return (s,ls,n)
        parseAlgorithm = do
            algo <- parseAnyString
            makeAlgorithm algo
        parseKernelLengths ::  P.Parser [KernelLengths]
        parseKernelLengths = do
            res <- parseNamedVector parseIndepVarName (P.try parseSequence P.<|> P.try parseList P.<|> parseSingle)
            let flattened = map (\(name,vs) -> map (name,) vs) res
                permutations = sequenceA flattened
            return $ map (KernelLengths . makeValuesPerIndepVar) permutations
        parseNugget = parsePositiveFloatNumber
        parseSequence = parseDoubleSequence
        parseList = parseVector parseDouble
        parseSingle = singleton <$> parseDouble

-- general parsers

parseIndepVarName :: P.Parser String
parseIndepVarName =
          P.string "space"
    P.<|> P.string "time"
    P.<|> P.string "indep" <> P.many1 P.alphaNum

parseDepVarName :: P.Parser String
parseDepVarName = P.string "dep" <> P.many1 P.alphaNum

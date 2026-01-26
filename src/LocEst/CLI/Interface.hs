{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module LocEst.CLI.Interface where

import           LocEst.CLI.ConfigLang
import           LocEst.CLI.Cross
import           LocEst.CLI.Search
import           LocEst.CLI.Serialise
import           LocEst.CLI.Vario
import           LocEst.Types
import           LocEst.Utils

import           Data.Char                (isSpace, toLower)
import           Data.List                (groupBy, isPrefixOf, singleton, sort)
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
                            <*> optParseDistFile
                            )) (OP.progDesc "Serialise self distance matrix.")) -- TODO: make less hacky
                     <> OP.command "crossdist" (OP.info (OP.helper <*> (
                            SerialiseCrossDistMatrixPerIndepVar
                            <$> optParseInObservationFile
                            <*> optParseInIndepVarGridFile
                            <*> optParseDistFile
                            )) (OP.progDesc "Serialise cross distance matrix.")) -- TODO: make less hacky
                     ) <*> optParseOutFileCbor

searchOptParser :: OP.Parser SearchOptions
searchOptParser = SearchOptions
                        <$> optParseInObservationFile
                        <*> OP.optional optParseInObsTempSamplesFile
                        <*> optParseInIndepVarGridFile
                        <*> OP.optional optParseTempGridString
                        <*> OP.optional optParseSearchPositions
                        <*> optParseKernDefString
                        <*> OP.optional optParseInObsGridDistFile
                        <*> OP.optional optParseInObsObsDistFile
                        <*> OP.optional optParseInGridGridDistFile
                        <*> optParseTopNObs
                        <*> optParseOutFile

varioOptParser :: OP.Parser VarioOptions
varioOptParser = VarioOptions
                        <$> optParseInObservationFile
                        <*> OP.optional optParseInObsObsDistFile
                        <*> optParseAcrossSettings
                        <*> optParseSpaceTimeScaling
                        <*> optParseIndepVarsThresholds
                        <*> optParseOutFile
                        <*> optParseVarioOutMode

crossOptParser :: OP.Parser CrossOptions
crossOptParser = CrossOptions
                        <$> optParseInObservationFile
                        <*> optParseKernDefStringPermutations
                        <*> optParseTestTrainingFraction
                        <*> optParseCrossvalIterations
                        <*> optParseCrossvalConfSeed
                        <*> OP.optional optParseInObsObsDistFile
                        <*> optParseOutFile


optParseInObsGridDistFile :: OP.Parser FilePath
optParseInObsGridDistFile = OP.strOption (
       OP.long    "obsGridDistFile"
    <> OP.metavar "FILE"
    <> OP.helpDoc ( Just (
                      s2d "Path to a .tsv/.cbor file with distances between pairs of observations and \
                          \prediction grid positions along arbitrary independent variables. \
                          \With this the given distances will not be calculated from the respective \
                          \coordinates, but looked up in this table. \
                          \The pairs must be ordered first by gridID (as in --gridFile) and then within \
                          \that by obsID (as in --obsFile). \
                          \The ID columns can be omitted - they are not read or validated."
    <> OH.hardline <>     "┌─────┬──────┬─────┬────┬───────┬───────┐"
    <> OH.hardline <>     "│obsID│gridID│space│time│indepV1│indepV2│ > [obsID] (optional):"
    <> OH.hardline <>     "├─────┼──────┼─────┼────┼───────┼───────┤   Observations identifier"
    <> OH.hardline <>     "│   a │    x │     │    │       │       │ > [gridID] (optional):"
    <> OH.hardline <>     "│   b │    x │     │    │       │       │   Grid position identifier"
    <> OH.hardline <>     "│   a │    y │     │    │       │       │ > [space]/[time]/[indepV*]:"
    <> OH.hardline <>     "│   b │    y │     │    │       │       │   Distances"
    <> OH.hardline <>     "└─────┴──────┴─────┴────┴───────┴───────┘"
    ))
    )

optParseInObsObsDistFile :: OP.Parser FilePath
optParseInObsObsDistFile = OP.strOption (
       OP.long    "obsObsDistFile"
    <> OP.metavar "FILE"
    <> OP.helpDoc ( Just (
                      s2d "Path to a .tsv/.cbor file with distances between pairs of observations \
                          \along arbitrary independent variables. \
                          \With this the given distances will not be calculated from the respective \
                          \coordinates, but looked up in this table. \
                          \The pairs must be ordered first by id1 (as in --obsFile) and then within \
                          \that by id2 (also as in --obsFile). Every pair must only be given once, \
                          \as the distances are symmetric. \
                          \The ID columns can be omitted - they are not read or validated."
    <> OH.hardline <>     "┌───┬───┬─────┬────┬───────┬───────┐"
    <> OH.hardline <>     "│id1│id2│space│time│indepV1│indepV2│ > [id1] (optional):"
    <> OH.hardline <>     "├───┼───┼─────┼────┼───────┼───────┤   Observations identifier"
    <> OH.hardline <>     "│ a │ a │     │    │       │       │ > [id2] (optional):"
    <> OH.hardline <>     "│ b │ a │     │    │       │       │   Observations identifier"
    <> OH.hardline <>     "│ b │ b │     │    │       │       │ > [space]/[time]/[indepV*]:"
    <> OH.hardline <>     "│ c │ a │     │    │       │       │   Distances"
    <> OH.hardline <>     "└───┴───┴─────┴────┴───────┴───────┘"
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
    <> OP.metavar "c(space=DOUBLE,time=DOUBLE,indepV1=DOUBLE,...)"
    <> OP.value (makeValuesPerIndepVar [])
    <> OP.helpDoc ( Just (
                      s2d "Thresholds for the filtering distances across independent variables. \
                          \When computing a variogram for temporal distances it might for example \
                          \be desirable to constraint the spatial distances, so that only observations \
                          \in spatial proximity are considered."
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
    OP.metavar "EqualSize(n=INT)|OneBinMax(max = c(indepV1=DOUBLE,indepV2=DOUBLE,...)" <>
    OP.value (BinByNrBins 100)
    <> OP.helpDoc ( Just (
                      s2d "The binning procedure that should be applied for the variogram. \
                          \The output of vario depends on the binning, but generally \
                          \it returns a table like this:"
    <> OH.hardline <>     "┌────────┬──────┬───────┬───────┬───────┬────────┐"
    <> OH.hardline <>     "│indepVar│depVar│bin_min|bin_mid|bin_max│variance│"
    <> OH.hardline <>     "├────────┼──────┼───────┼───────┼───────┼────────┤"
    <> OH.hardline <>     "│        │      │       │       │       |        |"
    <> OH.hardline <>     "└────────┴──────┴───────┴───────┴───────┴────────┘"
    <> OH.hardline <>     "> [indepVar]: Independent variable"
    <> OH.hardline <>     "> [depVar]: Dependent variable"
    <> OH.hardline <> s2d "> [bin_min,bin_mid,bin_max]: Start, center and end point of each \
                             \independent variable bin"
    <> OH.hardline <> s2d "> [variance]: Variance calculated for the dependent variable \
                             \based on all observations in the respective bin"
    <> OH.hardline <> s2d "EqualSize(n): Bins the observations into n bins with an equal amount of \
                          \observations."
    <> OH.hardline <> s2d "OneBinMax(max = c(indepV1=DOUBLE,indepV2=DOUBLE,...): Only create one bin \
                          \per independent and dependent variable with a given upper limit."
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
            res <- parseRecordType "EqualSize" $ do
                n <- parseArgument "n" parseInt
                return n
            return (BinByNrBins res)
        parseOneBinMax = do
            res <- parseRecordType "OneBinMax" $ do
                maxPerIndepVar <- parseArgument "max" (parseNamedVector parseIndepVarName parseDouble)
                return $ makeValuesPerIndepVar maxPerIndepVar
            return (BinForNugget res)

optParseCrossOutMode :: OP.Parser CrossOutModeSettings
optParseCrossOutMode = OP.option (OP.eitherReader readOutMode) (
    OP.long "outMode" <>
    OP.metavar "Summed|Obs" <>
    OP.value SummedLikelihoodPerKernelSetting
    <> OP.helpDoc ( Just (
                      s2d "The type of output that should be written to the --outFile."
    <> OH.hardline <> s2d "Summed (default): The individual crossvalidation iterations are \
                          \summarised to a short table with only the tested kernel parameter \
                          \settings and the summed crossvalidation output."
    <> OH.hardline <> s2d "Obs: The output is as --outMode Full for the search subcommand, but the \
                          \search observations (--searchObsFile) are set as the test fraction \
                          \of the crossvalidation data split. Each iteration is returned separately."
    ))
    )
    where
        readOutMode :: String -> Either String CrossOutModeSettings
        readOutMode s =
            case P.runParser parseOutMode () "" s of
                Left err -> Left $ showParsecErr err
                Right x  -> Right x
        parseOutMode = P.try parseSummed P.<|> parseObs
        parseSummed = P.string "Summed" >> return SummedLikelihoodPerKernelSetting
        parseObs    = P.string "Obs"    >> return IndividualSearchObsResults

optParseCrossvalConfSeed :: OP.Parser (Maybe Int)
optParseCrossvalConfSeed = OP.option (Just <$> OP.auto) (
       OP.long  "seed"
    <> OP.metavar "INT"
    <> OP.value Nothing
    <> OP.helpDoc ( Just (
                      s2d "Seed for the random number generator used to create test and training data \
                          \subsets. Default: A random seed (not reproducible)."
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
                              \The fraction must be between 0 and 1. Default: 0.2"
        <> OH.hardline <>     "When the fraction is so large that the number of test observations \
                              \equals the total number of observations, then all observations are used \
                              \for the training set. In this case the seed has no effect and all \
                              \iterations yield the same result."
    ))
    )
    where
        readFraction :: String -> Either String Double
        readFraction s =
            case P.runParser parseFraction () "" s of
                Left err -> Left $ showParsecErr err
                Right x  -> Right x

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
       OP.long    "topobs"
    <> OP.metavar "INT"
    <> OP.value 0
    <> OP.helpDoc ( Just (
                      s2d "When this is >0, then a list of n observations with the highest weight \
                          \is computed for each prediction grid point and dependent variable. \
                          \It is documented in an output column [grid_top_obs_<dependent_variable>]. \
                          \Default: 0"
    ))
    )

optParseAcrossIndepVars :: OP.Parser Bool
optParseAcrossIndepVars = OP.switch (
       OP.long "acrossIndepVars"
    <> OP.helpDoc ( Just (
                      s2d "Calculate the variogram for Euclidean distances across all independent variables."
    ))
    )

optParseAcrossSettings :: OP.Parser AcrossSettings
optParseAcrossSettings = OP.option (OP.eitherReader readAcrossSettings) (
    OP.long "across" <>
    OP.metavar "IndepVars|DepVars|Both|AllCombinations" <>
    OP.value AcrossNone
    <> OP.helpDoc ( Just (
                      s2d "..."
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
    <> OP.metavar "c(space = DOUBLE, time = DOUBLE)"
    <> OP.helpDoc ( Just (
                      s2d "Space-time scaling factors. All temporal and spatial distances will be multiplied by \
                          \the respective factors before combining the distances as one Euclidean distance. \
                          \Only relevant for the spatiotemporal setting. Default: c(space = 1, time = 1)."
    ))
    <> OP.value (1,1)
    )

optParseQuiet :: OP.Parser Bool
optParseQuiet = OP.switch (
    OP.long "quiet" <>
    OP.short 'q' <>
    OP.help "Suppress the printing of progress messages to the stderr stream on the command line."
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
                              \the interpolation. Columns for the basic spatiotemporal case:"
    <> OH.hardline <>    "┌─────┬───┬───┬────────┬─────┬─────┬──────┬───┐"
    <> OH.hardline <>    "│obsID│ x │ y │yearBCAD│depV1│depV2│dep...│...│"
    <> OH.hardline <>    "├─────┼───┼───┼────────┼─────┼─────┼──────┼───┤"
    <> OH.hardline <>    "│     │   │   │        │     │     │      │   │"
    <> OH.hardline <>    "└─────┴───┴───┴────────┴─────┴─────┴──────┴───┘"
    <> OH.hardline <> s2d "> [obsID]: Observation identifier"
    <> OH.hardline <> s2d "> [x, y, yearBCAD] or [longitude, langitude, yearBCAD] or \
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
                          \--obsFile, but looked up in this table. The pairs must be ordered like and by \
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

optParseDistFile :: OP.Parser FilePath
optParseDistFile = OP.strOption (
    OP.long "distFile" <>
    OP.metavar "FILE" <>
    OP.help "TODO")

optParseSearchPositions :: OP.Parser DepVarsPredGridSettings
optParseSearchPositions =
           DirectDepVarsGridSettings <$> optParseSearchDepVarsPos
    OP.<|> SearchObsDepVarsGridSettings <$> optParseInSearchObservationFile

readFilterThresholds :: String -> Either String (Either (Double, Double) ArbitraryDimThresholds)
readFilterThresholds s =
    case P.runParser parseIndepVarsThresholds () "" s of
        Left err -> Left $ showParsecErr err
        Right x  -> x
    where
        parseIndepVarsThresholds = do
            res <- parseNamedVector parseIndepVarName parsePositiveDouble
            return (makeSpatTempOrAbritraryDim res)
        makeSpatTempOrAbritraryDim :: [(String, Double)] -> Either String (Either (Double, Double) ArbitraryDimThresholds)
        makeSpatTempOrAbritraryDim xs
            | sort (map fst xs) == ["space", "time"] = Right $ Left $ tuplify xs
            | all (isPrefixOf "indep" . fst) xs      = Right $ Right $ makeValuesPerIndepVar xs
            | otherwise                              = Left "--indepMinFilter and --indepMaxFilter can fit \
                                                              \either to a spatiotemporal or a arbitrary variable setup"
        tuplify :: [(String,Double)] -> (Double,Double)
        tuplify [("space",spatialThreshold), ("time",temporalThreshold)] = (spatialThreshold,temporalThreshold)
        tuplify [("time",temporalThreshold), ("space",spatialThreshold)] = (spatialThreshold,temporalThreshold)
        tuplify _                                                        = throwL "this can not happen"

optParseInIndepVarGridFile :: OP.Parser FilePath
optParseInIndepVarGridFile = OP.strOption (
       OP.long    "gridFile"
    <> OP.short   'g'
    <> OP.metavar "FILE"
    <> OP.helpDoc ( Just (
                      s2d "Path to a .tsv/.cbor file with independent variable positions \
                          \(e.g. spatial coordinates) where interpolation and search should be performed."
    <> OH.hardline <>     "┌───┬───┐"
    <> OH.hardline <>     "│ x │ y │ > [x, y] or [longitude, latitude]"
    <> OH.hardline <>     "├───┼───┤   Spatial coordinates"
    <> OH.hardline <>     "│   │   │"
    <> OH.hardline <>     "└───┴───┘"
    <> OH.hardline <>     "OR"
    <> OH.hardline <>     "┌───────┬───────┬────────┐"
    <> OH.hardline <>     "│indepV1│indepV2│indep...│ > [indepV1, ...]:"
    <> OH.hardline <>     "├───────┼───────┼────────┤   Independent variable"
    <> OH.hardline <>     "│       │       │        │   positions"
    <> OH.hardline <>     "└───────┴───────┴────────┘"
    ))
    )

optParseTempGridString :: OP.Parser [AbsRelTempPos]
optParseTempGridString = OP.option (OP.eitherReader readTempGridString) (
       OP.long    "tempGrid"
    <> OP.short   't'
    <> OP.metavar "absolute|relative(years = YEAR|c(YEAR1,YEAR2,...)|START:STOP:BY)"
    <> OP.helpDoc ( Just (
                      s2d "Temporal positions in years BC/AD where interpolation and search should \
                          \be performed. absolute(...) means absolute years BC/AD. relative(...) only \
                          \works with --searchObsFile and means before or after the age of the respective \
                          \search sample. Negative integer numbers mark years BC or before the search sample, \
                          \positive numbers years AD or after the search sample. \
                          \A list of years can be defined in three ways:"
    <> OH.hardline <> s2d "> YEAR: One year, e.g. \"-3000\" for 3000BC"
    <> OH.hardline <> s2d "> c(YEAR1,YEAR2,...): A list of years, e.g. \"c(-3000, 1000)\" for 3000BC \
                          \and 1000AD"
    <> OH.hardline <> s2d "> START:STOP:BY: A sequence  of years, e.g. \"-3000:1000:2000\" for 3000BC, \
                          \1000BC and 1000AD"
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
    <> OP.metavar "c(depV1=DOUBLE,depV1=c(DOUBLE,DOUBLE,...),depV3=START:STOP:BY,...)"
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
                          \to stdout. See --outMode to set the desired type of output."
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
    <> OH.hardline <>     "│  algorithm = GPR,  │ interpolation algorithm"
    <> OH.hardline <>     "│                    │ - either GPR or KAS"
    <> OH.hardline <>     "│  depVars = c(      │ named list of dependent variables"
    <> OH.hardline <>     "│    depV1 = k(      │ - first dependent variable"
    <> OH.hardline <>     "│      shape = SqEx, │   - either SqEx = Squared exponential"
    <> OH.hardline <>     "│                    │         or Linear = Linear kernel"
    <> OH.hardline <>     "│      lengths = c(  │   - named list with length scale"
    <> OH.hardline <>     "│        space = ... │     for each independent variable"
    <> OH.hardline <>     "│        time = ...  │     (can also be \"indep...\")"
    <> OH.hardline <>     "│      ),            │"
    <> OH.hardline <>     "│      nugget = ...  │   - (optional) nugget parameter"
    <> OH.hardline <>     "│                    │     only relevant for GPR"
    <> OH.hardline <>     "│    ),              │"
    <> OH.hardline <>     "│    depV2 = k(...)  │ - second dependent variable"
    <> OH.hardline <>     "│  )                 │"
    <> OH.hardline <>     "│)                   │"
    <> OH.hardline <>     "└────────────────────┘"
    <> OH.hardline <> s2d "Any number of dependent and independent variables can be specified, but \
                          \all variables must also exist in --obsFile and --spatGridFile/--anyGridFile."
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
        parseKernelShapes = do
            shape <- parseAnyString
            makeKernelShape shape
        parseKernelLengths = KernelLengths . makeValuesPerIndepVar <$> parseNamedVector parseIndepVarName parseDouble
        parseNugget = parsePositiveFloatNumber

optParseCoAnalyseDepVars :: OP.Parser Bool
optParseCoAnalyseDepVars = OP.switch (
       OP.long "coAnalyseDepVars"
    <> OP.helpDoc ( Just (
                      s2d "Run the crossvalidation for all permutations of kernel parameters \
                          \of all dependent variables. This is computationally very expensive. \
                          \By default each dependent variable is analysed independently."
    ))
    )

optParseKernDefStringPermutations :: OP.Parser [KernelDefinition]
optParseKernDefStringPermutations = OP.option (OP.eitherReader readKernDefString) (
       OP.long    "algodef"
    <> OP.short   'a'
    <> OP.metavar "DSL"
    <> OP.helpDoc ( Just (
                      s2d "Algorithm parameter settings for the interpolation."
    <> OH.hardline <>     "┌────────────────────┐"
    <> OH.hardline <>     "│def(                │"
    <> OH.hardline <>     "│  algorithm = GPR,  │ interpolation algorithm"
    <> OH.hardline <>     "│                    │ - either GPR or KAS"
    <> OH.hardline <>     "│  depVars = c(      │ named list of dependent variables"
    <> OH.hardline <>     "│    depV1 = k(      │ - first dependent variable"
    <> OH.hardline <>     "│      shape = SqEx, │   - either SqEx = Squared exponential"
    <> OH.hardline <>     "│                    │         or Linear = Linear kernel"
    <> OH.hardline <>     "│      lengths = c(  │   - named list with length scale"
    <> OH.hardline <>     "│        space = ... │     for each independent variable *"
    <> OH.hardline <>     "│        time = ...  │     (can also be \"indep...\")"
    <> OH.hardline <>     "│      ),            │"
    <> OH.hardline <>     "│      nugget = ...  │   - (optional) nugget parameter"
    <> OH.hardline <>     "│                    │     only relevant for GPR"
    <> OH.hardline <>     "│    ),              │"
    <> OH.hardline <>     "│    depV2 = k(...)  │ - second dependent variable"
    <> OH.hardline <>     "│  )                 │"
    <> OH.hardline <>     "│)                   │"
    <> OH.hardline <>     "└────────────────────┘"
    <> OH.hardline <> s2d "Any number of dependent and independent variables can be specified, but \
                          \all variables must also exist in --obsFile and --spatGridFile/--anyGridFile."
    <> OH.hardline <> s2d "* Unlike for the search subcommand, here multiple values can be given for the \
                          \independent variables' length scale parameters. They can be provided \
                          \either as a list with \"c(100,200,...)\" or as a sequence using the \
                          \START:STOP:BY syntax, e.g. \"100:1000:100\". The crossvalidation will try \
                          \all permutations of these parameters."
    ))
    )
    where
        readKernDefString :: String -> Either String [KernelDefinition]
        readKernDefString s =
            case P.runParser parseAKernDefString () "" s of
                Left err -> Left $ showParsecErr err
                Right x  -> Right x
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
        parseKernelShapes = do
            shape <- parseAnyString
            makeKernelShape shape
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

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module LocEst.CLI.Interface where

import           LocEst.CLI.ConfigLang
import           LocEst.CLI.Cross
import           LocEst.CLI.Search
import           LocEst.CLI.Serialise
import           LocEst.CLI.Vario
import           LocEst.Exceptions
import           LocEst.Types

import           Data.Char                (isSpace, toLower)
import           Data.List                (groupBy, singleton)
import           LocEst.MathUtils         (infinity)
import qualified Options.Applicative      as OP
import qualified Options.Applicative.Help as OH
import qualified Text.Parsec              as P
import qualified Text.Parsec.String       as P
import           Text.Read                (readMaybe)

-- helper functions for optparse applicative help text

s2d :: String -> OH.Doc
s2d str = OH.fillSep $ map OH.pretty $ words str

-- config file that uses the optparse interface

parseConfigFile :: FilePath -> IO [String]
parseConfigFile configFile = do
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
        argumentValue <- P.manyTill (P.noneOf ";") (P.lookAhead (P.char ';'))
        _ <- P.char ';'
        _ <- P.try parseComment P.<|> parseEmptyLine
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
                     <> OP.command "spatgrid" (OP.info (OP.helper <*> (
                            SerialiseSpatGridFile
                            <$> optParseInSpatGridFile
                            )) (OP.progDesc "Serialise --spatGridFile."))
                     <> OP.command "anygrid" (OP.info (OP.helper <*> (
                            SerialiseSpatGridFile
                            <$> optParseInArbitraryDimFile
                            )) (OP.progDesc "Serialise --anyGridFile."))
                     <> OP.command "obsdist" (OP.info (OP.helper <*> (
                            SerialiseObsObsSpatDistFile
                            <$> optParseInSpatDistMapFile
                            <*> optParseInObservationFile
                            <*> optParseInSpatDistNoOrderCheck
                            )) (OP.progDesc "Serialise --spatDistFile for the observation-observation distance case in cross."))
                     <> OP.command "spatdist" (OP.info (OP.helper <*> (
                            SerialiseSpatDistFile
                            <$> optParseInSpatDistMapFile
                            <*> optParseInObservationFile
                            <*> optParseInSpatGridFile
                            <*> optParseInSpatDistNoOrderCheck
                            )) (OP.progDesc "Serialise --spatDistFile for the observation-spatial grid case in search."))
                     <> OP.command "tempsamp" (OP.info (OP.helper <*> (
                            SerialiseObsTempSamplesFile
                            <$> optParseInObservationFile
                            <*> optParseInObsTempSamplesFile
                            <*> optParseInSpatDistNoOrderCheck
                            )) (OP.progDesc "Serialise --tempSampFile."))
                     ) <*> optParseOutFileCbor

searchOptParser :: OP.Parser SearchOptions
searchOptParser = SearchOptions
                        <$> optParseInObservationFile
                        <*> optParseSearchGridSettings
                        <*> optParseKernDefString
                        <*> optParseNormalisation
                        <*> optParseOutFile
                        <*> optParseCoreOutMode

varioOptParser :: OP.Parser VarioOptions
varioOptParser = VarioOptions
                        <$> optParseInObservationFile
                        <*> OP.optional optParseSpatDistSetting
                        <*> optParseAcrossIndepVars
                        <*> optParseSpaceTimeScaling
                        <*> optParseIndepVarsThresholds
                        <*> optParseAcrossDepVars
                        <*> optParseOutFile
                        <*> optParseVarioOutMode

crossOptParser :: OP.Parser CrossOptions
crossOptParser = CrossOptions
                        <$> optParseInObservationFile
                        <*> optParseSpaceTimeCoreSupplementSettings
                        <*> optParseCrossSettings
                        <*> optParseOutFile
                        <*> optParseCrossOutMode

optParseSpatDistSetting :: OP.Parser SpatDistSettings
optParseSpatDistSetting = SpatDistSettings
                        <$> optParseInSpatDistMapFile
                        <*> optParseInSpatDistNoOrderCheck

optParseIndepVarsThresholds :: OP.Parser IndepVarsThresholds
optParseIndepVarsThresholds = OP.option (OP.eitherReader readOutMode) (
       OP.long "indepVarsThresholds"
    <> OP.metavar "c(indepV1=DOUBLE,indepV2=DOUBLE,...)"
    <> OP.value (ValuesPerIndepVar [])
    <> OP.helpDoc ( Just (
                      s2d "Thresholds for the filtering distances across independent variables. \
                          \When computing a variogram for temporal distances it might for example \
                          \be desirable to constraint the spatial distances, so that only observations \
                          \in spatial proximity are considered."
    ))
    )
    where
        readOutMode :: String -> Either String IndepVarsThresholds
        readOutMode s =
            case P.runParser parseIndepVarsThresholds () "" s of
                Left err -> Left $ showParsecErr err
                Right x  -> Right x
        parseIndepVarsThresholds = do
            res <- parseNamedVector parseIndepVarName parsePositiveDouble
            return (ValuesPerIndepVar res)

optParseVarioOutMode :: OP.Parser BinModeSettings
optParseVarioOutMode = OP.option (OP.eitherReader readOutMode) (
    OP.long "outMode" <>
    OP.metavar "EqualSize(n=INT)|OneBinMax(max = c(indepV1=DOUBLE,indepV2=DOUBLE,...)" <>
    OP.value (BinByNrBins 100)
    <> OP.helpDoc ( Just (
                      s2d "The binning procedure that should be applied for the variogram. \
                          \The output of vario depends to some degree on the binning, but generally \
                          \it returns a table like this:"
    <> OH.hardline <>     "┌────────┬──────┬───┬────────┐"
    <> OH.hardline <>     "│indepVar│depVar│bin│variance│"
    <> OH.hardline <>     "├────────┼──────┼───┼────────┤"
    <> OH.hardline <>     "│        │      │   │        │"
    <> OH.hardline <>     "└────────┴──────┴───┴────────┘"
    <> OH.hardline <>     "> [indepVar]: Independent variable"
    <> OH.hardline <>     "> [depVar]: Dependent variable"
    <> OH.hardline <>     "> [bin]: center point of each independent variable bin"
    <> OH.hardline <> s2d "> [variance]: variance calculated for the dependent variable \
                          \based on all observations in the respective bin"
    <> OH.hardline <> s2d "EqualSize(n): Bins the observations into n bins with an equal amount of \
                          \observations."
    <> OH.hardline <> s2d "Nugget(max = c(indepV1=DOUBLE,indepV2=DOUBLE,...): Only create one bin \
                          \per independent and dependent variable with a given upper limit. \
                          \This is useful to get an estimate for the nugget parameter."
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
            res <- parseRecordType "Nugget" $ do
                maxPerIndepVar <- parseArgument "max" (parseNamedVector parseIndepVarName parseDouble)
                return $ ValuesPerIndepVar maxPerIndepVar
            return (BinForNugget res)

optParseCrossSettings :: OP.Parser CrossSettings
optParseCrossSettings =
    CrossSettings
    <$> optParseKernDefStringPermutations
    <*> optParseCrossSubsetMode

optParseCrossSubsetMode :: OP.Parser CrossSubsetMode
optParseCrossSubsetMode = optParseCrossFull OP.<|> optParseCrossFraction

optParseCrossFull :: OP.Parser CrossSubsetMode
optParseCrossFull = OP.flag' CrossFull (
       OP.long "full"
    <> OP.helpDoc ( Just (
                      s2d "Use all input observations both as test and training data. \
                          \This is faster than running through multiple test-training split iterations, \
                          \but potentially also less reliable."
    )))

optParseCrossFraction :: OP.Parser CrossSubsetMode
optParseCrossFraction = CrossFraction
                    <$> optParseTestTrainingFraction
                    <*> optParseCrossvalIterations
                    <*> optParseCrossvalConfSeed

optParseCrossOutMode :: OP.Parser CrossOutModeSettings
optParseCrossOutMode = OP.option (OP.eitherReader readOutMode) (
    OP.long "outMode" <>
    OP.metavar "Summed|Obs" <>
    OP.value SummedLikelihoodPerKernelSetting
    <> OP.helpDoc ( Just (
                      s2d "The type of output that should be written to the --outFile. \
                          \For Summed (default) the individual crossvalidation iterations are \
                          \summarised to a short table with only the tested kernel parameter \
                          \settings and the summed crossvalidation output:"
    <> OH.hardline <>     "Kernel parameter settings            "
    <> OH.hardline <>     "┌──────┬─────┬──────────────┐"
    <> OH.hardline <>     "│kernel│depV1│shape         │ From --kerndef:"
    <> OH.hardline <>     "│      │depV2│nugget        │ Kernel shape and"
    <> OH.hardline <>     "│      │...  ├───────┬──────┤ nugget for each"
    <> OH.hardline <>     "│      │     │space  │length│ dependent variable;"
    <> OH.hardline <>     "│      │     │time OR│      │ lengthscale"
    <> OH.hardline <>     "│      │     │indepV1│      │ parameters for"
    <> OH.hardline <>     "│      │     │indepV2│      │ each dependent and"
    <> OH.hardline <>     "│      │     │...    │      │ independent one"
    <> OH.hardline <>     "└──────┴─────┴───────┴──────┘"
    <> OH.hardline <>     "Crossvalidation result               "
    <> OH.hardline <>     "┌───────────────────────────────┐"
    <> OH.hardline <>     "│sum_dep_dist_euclidean         │ Distance to and"
    <> OH.hardline <>     "│mean_squared_dep_dist_euclidean│ likelihood of test"
    <> OH.hardline <>     "│sum_log_likelihood             │"
    <> OH.hardline <>     "└───────────────────────────────┘"
    <> OH.hardline <> s2d "With Obs the output is as --outMode Full for the search subcommand \
                          \where the search observations (--searchObsFile) are set as the test fraction \
                          \of the crossvalidation data split. Here each iteration is given separately."
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

optParseCoreOutMode :: OP.Parser CoreOutMode
optParseCoreOutMode = OP.option (OP.eitherReader readOutMode) (
    OP.long "outMode" <>
    OP.metavar "Short|Full|Obs(n)|Samples(n,seed)" <>
    OP.value CoreOutShort
    <> OP.helpDoc ( Just (
                      s2d "The type of output that should be written to the --outFile. \
                          \For Short (default) and Full the following columns are produced:"
    <> OH.hardline <>     "Prediction position                  "
    <> OH.hardline <>     "┌──────────────┐    ┌───────┐"
    <> OH.hardline <>     "│spatID        │    │indepV1│ Prediction grid"
    <> OH.hardline <>     "│x or longitude│ OR │indepV2│ for the spatio-"
    <> OH.hardline <>     "│y or latitude │    │...    │ temporal or the"
    <> OH.hardline <>     "│yearBCAD      │    │       │ any space case"
    <> OH.hardline <>     "└──────────────┘    └───────┘"
    <> OH.hardline <>     "Search positions *                   "
    <> OH.hardline <>     "┌──────┬────────────────────┐"
    <> OH.hardline <>     "│search│depV1               │ with"
    <> OH.hardline <>     "│      │depV2               │ --searchDepVarsPos"
    <> OH.hardline <>     "│      │...                 │"
    <> OH.hardline <>     "│      │OR                  │"
    <> OH.hardline <>     "│      │the input columns   │ with"
    <> OH.hardline <>     "│      │from --obsFile      │ --searchObsFile"
    <> OH.hardline <>     "└──────┴────────────────────┘"
    <> OH.hardline <>     "Kernel parameter settings            "
    <> OH.hardline <>     "┌──────┬─────┬──────────────┐"
    <> OH.hardline <>     "│kernel│depV1│shape         │ From --kerndef:"
    <> OH.hardline <>     "│      │depV2│nugget        │ Kernel shape and"
    <> OH.hardline <>     "│      │...  ├───────┬──────┤ nugget for each"
    <> OH.hardline <>     "│      │     │space  │length│ dependent variable;"
    <> OH.hardline <>     "│      │     │time OR│      │ lengthscale"
    <> OH.hardline <>     "│      │     │indepV1│      │ parameters for"
    <> OH.hardline <>     "│      │     │indepV2│      │ each dependent and"
    <> OH.hardline <>     "│      │     │...    │      │ independent one"
    <> OH.hardline <>     "└──────┴─────┴───────┴──────┘"
    <> OH.hardline <>     "Temporal resampling iteration counter"
    <> OH.hardline <>     "┌───────────────────────────┐"
    <> OH.hardline <>     "│temp_sampling_iteration    │"
    <> OH.hardline <>     "└───────────────────────────┘"
    <> OH.hardline <>     "Interpolation output                 "
    <> OH.hardline <>     "┌────────┬─────┬────────────┐"
    <> OH.hardline <>     "│interpol│depV1│neff        │ See supplementary"
    <> OH.hardline <>     "│        │depV2│avg         │ documentation for"
    <> OH.hardline <>     "│        │...  │var         │ how these values"
    <> OH.hardline <>     "│        │     │low +       │ are calculated"
    <> OH.hardline <>     "│        │     │median +    │"
    <> OH.hardline <>     "│        │     │up +        │"
    <> OH.hardline <>     "│        │     │logl *      │"
    <> OH.hardline <>     "│        │     │prob *%     │"
    <> OH.hardline <>     "└────────┴─────┴────────────┘"
    <> OH.hardline <>     "Search results *                     "
    <> OH.hardline <>     "┌───────────────────────────┐"
    <> OH.hardline <>     "│dep_dist_euclidean         │ Summary search"
    <> OH.hardline <>     "│log_likelihood             │ results across"
    <> OH.hardline <>     "│probability %              │ all variables"
    <> OH.hardline <>     "└───────────────────────────┘"
    <> OH.hardline <>     "* for the search case                "
    <> OH.hardline <> s2d "+ with --outMode Short only these interpol_... variables are returned"
    <> OH.hardline <>     "% when normalisation is active       "
    <> OH.hardline <> s2d "With Obs(n) the n input observations with the heighest weight for the \
                          \prediction grid point (summed across dependent variables) are returned. \
                          \In this case the sections Interpolation output and Search results are \
                          \replaced by a section with the columns and weights of the respective \
                          \observations."
    ))
    )
    where
        readOutMode :: String -> Either String CoreOutMode
        readOutMode s =
            case P.runParser parseOutMode () "" s of
                Left err -> Left $ showParsecErr err
                Right x  -> Right x
        parseOutMode = P.try parseShort P.<|> parseFull P.<|> parseObs P.<|> parseInterpolSample
        parseShort = P.string "Short" >> return CoreOutShort
        parseFull  = P.string "Full"  >> return CoreOutFull
        parseObs   = do
            parseRecordType "Obs" $ do
                n <- parseArgument "n" parseInt
                return $ CoreOutObsWeight n
        parseInterpolSample = do
            parseRecordType "Samples" $ do
                n <- parseArgument "n" parseInt
                s <- parseArgumentOptional "seed" parseInt
                r <- parseArgumentOptional "range" parseSamplingRange
                return $ CoreOutInterpolSamples n s r
        parseSamplingRange = P.try parseOneSigma P.<|> P.try parseTwoSigma P.<|> parseFullDistribution
        parseOneSigma = P.string "1sigma" >> return OneSigma
        parseTwoSigma = P.string "2sigma" >> return TwoSigma
        parseFullDistribution = P.string "Full" >> return FullDistribution

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
                      s2d "Fraction of the observations that should be used as test data for the crossvalidation. \
                          \1 - testFraction will be used as training data. The fraction must be between 0 and 1. \
                          \Default: 0.2"
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

optParseAcrossIndepVars :: OP.Parser Bool
optParseAcrossIndepVars = OP.switch (
       OP.long "acrossIndepVars"
    <> OP.helpDoc ( Just (
                      s2d "Calculate the variogram for Euclidean distances across all independent variables. \
                          \For the spatiotemporal setting this assumes the space-time in --spaceTimeScale."
    ))
    )

optParseSpaceTimeScaling :: OP.Parser (Double,Double)
optParseSpaceTimeScaling = OP.option (OP.eitherReader readSpaceTime) (
       OP.long "spaceTimeScaling"
    <> OP.metavar "DOUBLE"
    <> OP.metavar "c(space = DOUBLE, time = DOUBLE)"
    <> OP.helpDoc ( Just (
                      s2d "Space-time scaling factors. All temporal and spatial distances will be multiplied by \
                          \the respective factors before combining the distances as one Euclidean distance. \
                          \Only relevant for the spatiotemporal setting. Default: scaling(space = 1, time = 1)"
    ))
    <> OP.value (1,1)
    )

optParseAcrossDepVars :: OP.Parser Bool
optParseAcrossDepVars = OP.switch (
       OP.long "acrossDepVars"
    <> OP.helpDoc ( Just (
                      s2d "Calculate the variogram for Euclidean distances across all dependent variables."
    ))
    )

optParseNormalisation :: OP.Parser Normalisation
optParseNormalisation = OP.option (OP.eitherReader readNormalisation) (
    OP.long "normalisation" <>
    OP.metavar "NoNorm|NormBySpace" <>
    OP.value NoNorm
    <> OP.helpDoc ( Just (
                      s2d "Should the output likelihoods from the search algorithm should be normalised? \
                          \Normalisation adds a column [probability] to the output table."
    <> OH.hardline <> s2d "NoNorm (default): Apply no normalisation."
    <> OH.hardline <> s2d "NormBySpace: Normalise across all spatial positions at one point in time \
                          \so across one \"time slice\". Only relevant for spatiotemporal interpolation."
    ))
    )
    where
        readNormalisation :: String -> Either String Normalisation
        readNormalisation s =
            case P.runParser parseNormalisation () "" s of
                Left err -> Left $ showParsecErr err
                Right x  -> Right x
        parseNormalisation = P.try parseNormBySpace P.<|> parseNoNorm
        parseNormBySpace = P.string "NormBySpace" >> return NormBySpace
        parseNoNorm      = P.string "NoNorm"      >> return NoNorm

optParseNumberOfThreads :: OP.Parser NumberOfThreads
optParseNumberOfThreads = OP.option (OP.eitherReader readNumberOfThreads) (
    OP.long "threads" <>
    OP.metavar "INT|Detect" <>
    OP.value SingleThread
    <> OP.help "Maximum number of worker threads. Either just an integer number to request \
               \a concrete number of threads or \"Detect\" to automatically determine the maximum. \
               \The default is to use one thread."
    ) where
        readNumberOfThreads :: String -> Either String NumberOfThreads
        readNumberOfThreads s = do
            if s == "Detect"
            then Right DetectThreads
            else case readMaybe s of
                Just n  -> Right $ MultipleThreads n
                Nothing -> Left "must be either \"Detect\" or an integer number"

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

optParseSearchGridSettings :: OP.Parser SearchGridSettings
optParseSearchGridSettings =
    SearchGridSettings
        <$> optParseIndepVarsPredGridSettings
        <*> OP.optional optParseSearchPositions

optParseSearchPositions :: OP.Parser DepVarsPredGridSettings
optParseSearchPositions =
           DirectDepVarsGridSettings <$> optParseSearchDepVarsPos
    OP.<|> SearchObsDepVarsGridSettings <$> optParseInSearchObservationFile

optParseIndepVarsPredGridSettings :: OP.Parser IndepVarsPredGridSettings
optParseIndepVarsPredGridSettings =
    (SpaceTimeGridSettings
        <$> optParseInSpatGridFile
        <*> optParseTempGridString
        <*> optParseSpaceTimeCoreSupplementSettings
    ) OP.<|>
    (ArbitraryDimGridSettings <$> optParseInArbitraryDimFile)

optParseSpaceTimeCoreSupplementSettings :: OP.Parser SpaceTimeCoreSupplementSettings
optParseSpaceTimeCoreSupplementSettings =
    SpaceTimeCoreSupplementSettings
        <$> optParseSpaceTimeMinFilter
        <*> optParseSpaceTimeMaxFilter
        <*> OP.optional optParseInSpatDistMapFile
        <*> OP.optional optParseInObsTempSamplesFile
        <*> optParseInSpatDistNoOrderCheck

optParseSpaceTimeMinFilter :: OP.Parser (Double,Double)
optParseSpaceTimeMinFilter = OP.option (OP.eitherReader readSpaceTime) (
       OP.long    "spaceTimeMinFilter"
    <> OP.metavar "c(space = DOUBLE, time = DOUBLE)"
    <> OP.value (0,0)
    <> OP.helpDoc ( Just (
                      s2d "Spatiotemporal radius filter. Only consider observations above \
                          \a certain minimum distance for the interpolation at the prediction \
                          \grid points."
    ))
    )

optParseSpaceTimeMaxFilter :: OP.Parser (Double,Double)
optParseSpaceTimeMaxFilter = OP.option (OP.eitherReader readSpaceTime) (
       OP.long    "spaceTimeMaxFilter"
    <> OP.metavar "c(space = DOUBLE, time = DOUBLE)"
    <> OP.value (infinity,infinity)
    <> OP.helpDoc ( Just (
                      s2d "Spatiotemporal radius filter. Only consider observations below \
                          \a certain maximum distance."
    ))
    )

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

optParseInSpatDistMapFile :: OP.Parser FilePath
optParseInSpatDistMapFile = OP.strOption (
       OP.long    "spatDistFile"
    <> OP.metavar "FILE"
    <> OP.helpDoc ( Just (
                      s2d "Path to a .tsv/.cbor file with spatial distances between pairs of observations and spatial \
                          \prediction grid points. If this is given, then the spatial distances will \
                          \not be calculated from the respective coordinates, but looked up in this \
                          \table. The pairs must be ordered first like and by --obsFile and then like \
                          \and by --spatGridFile."
    <> OH.hardline <>     "┌─────┬──────┬────┐"
    <> OH.hardline <>     "│obsID│spatID│dist│ > [obsID]:"
    <> OH.hardline <>     "├─────┼──────┼────┤   Observations identifier"
    <> OH.hardline <>     "│   a │    x │    │ > [spatID]:"
    <> OH.hardline <>     "│   a │    y │    │   Spatial coordinate identifier"
    <> OH.hardline <>     "│   b │    x │    │ > [dist]:"
    <> OH.hardline <>     "│   b │    y │    │   Spatial distance"
    <> OH.hardline <>     "└─────┴──────┴────┘"
    ))
    )

optParseInSpatDistNoOrderCheck :: OP.Parser Bool
optParseInSpatDistNoOrderCheck = OP.switch (
    OP.long "noOrderCheck"
    <> OP.helpDoc ( Just (
                    s2d "The input files --spatDistFile and --tempSampFile undergo an order validation \
                        \when read from .tsv (not from .cbor!). This validation is computationally \
                        \expensive for large files and can be turned off with this flag to speed up the reading. \
                        \This should only be set if the order is certainly correct, e.g. if it was \
                        \validated previously."
    ))
    )

optParseInArbitraryDimFile :: OP.Parser FilePath
optParseInArbitraryDimFile = OP.strOption (
       OP.long    "anyGridFile"
    <> OP.metavar "FILE"
    <> OP.helpDoc ( Just (
                      s2d "Path to a .tsv/.cbor file with arbitrary dimension coordinates where interpolation \
                          \should be performed."
    <> OH.hardline <>     "┌───────┬───────┬────────┐"
    <> OH.hardline <>     "│indepV1│indepV2│indep...│ > [indepV1, ...]:"
    <> OH.hardline <>     "├───────┼───────┼────────┤   Independent variable"
    <> OH.hardline <>     "│       │       │        │   position"
    <> OH.hardline <>     "└───────┴───────┴────────┘"
    ))
    )

optParseInSpatGridFile :: OP.Parser FilePath
optParseInSpatGridFile = OP.strOption (
       OP.long    "spatGridFile"
    <> OP.short   'g'
    <> OP.metavar "FILE"
    <> OP.helpDoc ( Just (
                      s2d "Path to a .tsv/.cbor file with spatial coordinates where interpolation \
                          \and search should be performed."
    <> OH.hardline <>     "┌──────┬───┬───┐"
    <> OH.hardline <>     "│spatID│ x │ y │ > [spatID]:"
    <> OH.hardline <>     "├──────┼───┼───┤   Spatial coordinate identifier"
    <> OH.hardline <>     "│      │   │   │ > [x, y] or [longitude, latitude]"
    <> OH.hardline <>     "│      │   │   │   Spatial coordinates"
    <> OH.hardline <>     "└──────┴───┴───┘"
    ))
    )

optParseTempGridString :: OP.Parser [AbsRelTempPos]
optParseTempGridString = OP.option (OP.eitherReader readTempGridString) (
       OP.long    "tempGrid"
    <> OP.short   't'
    <> OP.metavar "absolute(years = YEAR|c(YEAR1,YEAR2,...)|START:STOP:BY) | relative(years = ...)"
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
            return $ map ValuesPerDepVar permutations
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
       OP.long    "kerndef"
    <> OP.short   'k'
    <> OP.metavar "DSL"
    <> OP.helpDoc ( Just (
                      s2d "Kernel parameter settings for the interpolation."
    <> OH.hardline <>     "┌──────────────────┐"
    <> OH.hardline <>     "│c(                │ named list of dependent variables"
    <> OH.hardline <>     "│  depV1 = k(      │ - first dependent variable"
    <> OH.hardline <>     "│    shape = SqEx, │   - either SqEx = Squared exponential"
    <> OH.hardline <>     "│                  │         or Linear = Linear kernel"
    <> OH.hardline <>     "│    nugget = ..., │   - nugget parameter"
    <> OH.hardline <>     "│    lengths = c(  │   - named list with length scale"
    <> OH.hardline <>     "│      space = ... │     for each independent variable"
    <> OH.hardline <>     "│      time = ...  │     (can also be \"indep...\")"
    <> OH.hardline <>     "│    )             │"
    <> OH.hardline <>     "│  ),              │"
    <> OH.hardline <>     "│  depV2 = k(...)  │ - second dependent variable"
    <> OH.hardline <>     "│)                 │"
    <> OH.hardline <>     "└──────────────────┘"
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
                    nested <- parseNamedVector parseDepVarName parseShapeNuggetLengths
                    return $ makeKernelDefinition $ map (\(name,(s,n,l)) -> KernelOneDepVar name s n l) nested
        parseShapeNuggetLengths = do
            parseRecordType "k" $ do
                s <- parseArgument "shape" parseKernelShapes
                n <- parseArgument "nugget" parseDouble
                l <- parseArgument "lengths" parseKernelLengths
                return (s,n,l)
        parseKernelShapes = do
            shape <- parseAnyString
            makeKernelShape shape
        parseKernelLengths = KernelLengths . ValuesPerIndepVar <$> parseNamedVector parseIndepVarName parseDouble

optParseKernDefStringPermutations :: OP.Parser [KernelDefinition]
optParseKernDefStringPermutations = OP.option (OP.eitherReader readKernDefString) (
       OP.long    "kerndef"
    <> OP.short   'k'
    <> OP.metavar "DSL"
    <> OP.helpDoc ( Just (
                      s2d "Kernel parameter settings that should be tested with the crossvalidation. \
                          \This follows the following syntax:"
    <> OH.hardline <>     "┌──────────────────┐"
    <> OH.hardline <>     "│c(                │- named list of dependent variables"
    <> OH.hardline <>     "│  depV1 = k(      │- first dependent variable"
    <> OH.hardline <>     "│    shape = SqEx, │- either SqEx = Squared exponential"
    <> OH.hardline <>     "│                  │      or Linear = Linear kernel"
    <> OH.hardline <>     "│    nugget = ..., │- nugget parameters *"
    <> OH.hardline <>     "│    lengths = c(  │- named list with lengthscales"
    <> OH.hardline <>     "│      space = ... │  for each independent variable *"
    <> OH.hardline <>     "│      time = ...  │"
    <> OH.hardline <>     "│    )             │"
    <> OH.hardline <>     "│  ),              │"
    <> OH.hardline <>     "│  depV2 = k(...)  │- second dependent variable"
    <> OH.hardline <>     "│)                 │"
    <> OH.hardline <>     "└──────────────────┘"
    <> OH.hardline <> s2d "Any number of dependent and independent variables can be specified like this.\
                          \ \"space\" and \"time\" are a special case for the independent variable. \
                          \Use \"indepV1\", \"indepV2\", etc. for the arbitrary variables case, where\
                          \ \"V1\" and \"V2\" can be any name."
    <> OH.hardline <> s2d "All variables descripted here must also exist in the input in --obsFile."
    <> OH.hardline <> s2d "* Unlike for search, in cross multiple values can be given for the nugget \
                          \and the independent variables' lengthscale parameters. They can be provided \
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
                    perDepVar <- parseNamedVector parseDepVarName parseShapeNuggetLengths
                    let flattened = map (\(name,(s,ns,ls)) -> map (\(n,l) -> KernelOneDepVar name s n l) (allCombinations ns ls)) perDepVar
                        permutations = sequenceA flattened
                    return $ map KernelDefinition permutations
        parseShapeNuggetLengths = do
            parseRecordType "k" $ do
                s <- parseArgument "shape" parseKernelShapes
                ns <- parseArgument "nugget" parseNugget
                ls <- parseArgument "lengths" parseKernelLengths
                return (s,ns,ls)
        parseKernelShapes = do
            shape <- parseAnyString
            makeKernelShape shape
        parseNugget :: P.Parser [Double]
        parseNugget = P.try parseSequence P.<|> P.try parseList P.<|> parseSingle
        parseKernelLengths ::  P.Parser [KernelLengths]
        parseKernelLengths = do
            res <- parseNamedVector parseIndepVarName (P.try parseSequence P.<|> P.try parseList P.<|> parseSingle)
            let flattened = map (\(name,vs) -> map (name,) vs) res
                permutations = sequenceA flattened
            return $ map (KernelLengths . ValuesPerIndepVar) permutations
        parseSequence = parseDoubleSequence
        parseList = parseVector parseDouble
        parseSingle = singleton <$> parseDouble
        allCombinations xs ys = [ (x,y) | x <- xs, y <- ys ]

-- general parsers

parseIndepVarName :: P.Parser String
parseIndepVarName =
          P.string "space"
    P.<|> P.string "time"
    P.<|> P.string "indep" <> P.many1 P.alphaNum

parseDepVarName :: P.Parser String
parseDepVarName = P.string "dep" <> P.many1 P.alphaNum

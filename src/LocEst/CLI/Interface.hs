{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE OverloadedStrings #-}

module LocEst.CLI.Interface where

import           LocEst.CLI.ConfigLang
import           LocEst.CLI.Cross
import           LocEst.CLI.Search
import           LocEst.CLI.Serialise
import           LocEst.CLI.Vario
import           LocEst.Exceptions
import           LocEst.Types

import           Data.Char             (isSpace, toLower)
import           Data.List             (groupBy, singleton)
import qualified Options.Applicative   as OP
import qualified Text.Parsec           as P
import qualified Text.Parsec.String    as P
import           Text.Read             (readMaybe)
import qualified Options.Applicative.Help     as OH

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
                     ) <*> optParseOutFile

searchOptParser :: OP.Parser SearchOptions
searchOptParser = SearchOptions
                        <$> optParseInObservationFile
                        <*> optParseSearchGridSettings
                        <*> optParseKernDefString
                        <*> optParseNormalization
                        <*> optParseNumberOfThreads
                        <*> optParseCoreOutMode
                        <*> optParseOutFile

varioOptParser :: OP.Parser VarioOptions
varioOptParser = VarioOptions
                        <$> optParseInObservationFile
                        <*> OP.optional optParseSpatDistSetting
                        <*> optParseBinModeSettings
                        <*> optParseAcrossIndepVars
                        <*> optParseAcrossDepVars
                        <*> optParseNumberOfThreads
                        <*> optParseVariogramOutFile

crossOptParser :: OP.Parser CrossOptions
crossOptParser = CrossOptions
                        <$> optParseInObservationFile
                        <*> optParseSpaceTimeCoreSupplementSettings
                        <*> optParseCrossSettings
                        <*> optParseNumberOfThreads
                        <*> optParseCrossOutMode
                        <*> optParseOutFile

optParseSpatDistSetting :: OP.Parser SpatDistSettings
optParseSpatDistSetting = SpatDistSettings
                        <$> optParseInSpatDistMapFile
                        <*> optParseInSpatDistNoOrderCheck

optParseBinModeSettings :: OP.Parser BinModeSettings
optParseBinModeSettings = OP.option (OP.eitherReader readOutMode) (
    OP.long "outMode" <>
    OP.metavar "EqualSize(n=INT)|OneBinMax(max = c(indepX=DOUBLE,indepY=DOUBLE,...))" <>
    OP.help "..." <>
    OP.value (BinByNrBins 100) <>
    OP.showDefault
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
                return $ ValuesPerIndepVar maxPerIndepVar
            return (BinForNugget res)

optParseCrossSettings :: OP.Parser CrossSettings
optParseCrossSettings =
    CrossSettings
        <$> optParseKernDefStringPermutations
        <*> optParseTestTrainingFraction
        <*> optParseCrossvalIterations
        <*> optParseCrossvalConfSeed

optParseCrossOutMode :: OP.Parser CrossOutModeSettings
optParseCrossOutMode = OP.option (OP.eitherReader readOutMode) (
    OP.long "outMode" <>
    OP.metavar "Summed|Obs" <>
    OP.help "Output options." <>
    OP.value SummedLikelihoodPerKernelSetting <>
    OP.showDefault
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
    OP.metavar "Short|Full|Obs(n)" <>
    OP.help "Output options." <>
    OP.value CoreOutShort <>
    OP.showDefault
    )
    where
        readOutMode :: String -> Either String CoreOutMode
        readOutMode s =
            case P.runParser parseOutMode () "" s of
                Left err -> Left $ showParsecErr err
                Right x  -> Right x
        parseOutMode = P.try parseShort P.<|> parseFull P.<|> parseObs
        parseShort = P.string "Short" >> return CoreOutShort
        parseFull  = P.string "Full"  >> return CoreOutFull
        parseObs   = do
            res <- parseRecordType "Obs" $ do
                s <- parseArgument "n" parseInt
                return s
            return (CoreOutObsWeight res)

optParseCrossvalConfSeed :: OP.Parser (Maybe Int)
optParseCrossvalConfSeed = OP.option (Just <$> OP.auto) (
       OP.long  "seed"
    <> OP.metavar "INT"
    <> OP.help  "Seed for the random number generator for group splitting. \
                \The default causes locest to fall back to a random seed."
    <> OP.value Nothing
    <> OP.showDefault
    )

optParseTestTrainingFraction :: OP.Parser Double
optParseTestTrainingFraction = OP.option (OP.eitherReader readFraction) (
       OP.long    "testFraction"
    <> OP.metavar "DOUBLE"
    <> OP.help    "Fraction of the observations that should be used as test data for the crossvalidation.\
                  \ 1 - testFraction will be used as training data. The fraction must be between 0 and 1."
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
    <> OP.help    "Number of crossvalidation iterations, so how often should the input observations be\
                  \ reshuffled and split into test and training data for each kernel parameter set."
    )

optParseAcrossIndepVars :: OP.Parser Bool
optParseAcrossIndepVars = OP.switch (
    OP.long "acrossIndepVars" <>
    OP.help "Calculate the variogram for Euclidean distances across all independent variables.\
             \ Only applies for the arbitrary dimension setting, not the spatiotemporal setting."
    )

optParseAcrossDepVars :: OP.Parser Bool
optParseAcrossDepVars = OP.switch (
    OP.long "acrossDepVars" <>
    OP.help "Calculate the variogram for Euclidean distances across all dependent variables."
    )

optParseNormalization :: OP.Parser Normalization
optParseNormalization = OP.option (OP.eitherReader readNormalization) (
    OP.long "normalization" <>
    OP.metavar "NormBySpace|NoNorm" <>
    OP.help "How the output probabilities should be normalized." <>
    OP.value NoNorm <>
    OP.showDefault
    )
    where
        readNormalization :: String -> Either String Normalization
        readNormalization s =
            case P.runParser parseNormalization () "" s of
                Left err -> Left $ showParsecErr err
                Right x  -> Right x
        parseNormalization = P.try parseNormBySpace P.<|> parseNoNorm
        parseNormBySpace = P.string "NormBySpace" >> return NormBySpace
        parseNoNorm      = P.string "NoNorm"      >> return NoNorm

optParseNumberOfThreads :: OP.Parser NumberOfThreads
optParseNumberOfThreads = OP.option (OP.eitherReader readNumberOfThreads) (
    OP.long "threads" <>
    OP.metavar "INT|Detect" <>
    OP.help "Maximum number of worker threads." <>
    OP.value SingleThread <>
    OP.showDefault
    ) where
        readNumberOfThreads :: String -> Either String NumberOfThreads
        readNumberOfThreads s = do
            if s == "Detect"
            then Right DetectThreads
            else case readMaybe s of
                Just n  -> Right $ MultipleThreads n
                Nothing -> Left "must be either \"Detect\" or an integer number"

optParseInObservationFile :: OP.Parser FilePath
optParseInObservationFile = OP.strOption (
       OP.long    "obsFile"
    <> OP.short   'i'
    <> OP.metavar "FILE"
    <> OP.helpDoc ( Just (
                          s2d "Path to a .tsv/.cbor file with the input observations that should inform \
                              \the field. Columns:"
    <> OH.hardline <>    "┌───────┬───┬───┬──────────┬───────┬───────┐"
    <> OH.hardline <>    "│ obsID │ x │ y │ yearBCAD │ depC1 │ depC2 │"
    <> OH.hardline <>    "├───────┼───┼───┼──────────┼───────┼───────┤"
    <> OH.hardline <>    "│       │   │   │          │       │       │"
    <> OH.hardline <>    "│       │   │   │          │       │       │"
    <> OH.hardline <>    "└───────┴───┴───┴──────────┴───────┴───────┘"
    <> OH.hardline <> s2d "> [obsID]: Observation identifier"
    <> OH.hardline <> s2d "> [x, y, yearBCAD] or [longitude, langitude, yearBCAD] or \
                          \[indepC1, indepC2, ...]: Independent variable position where the \
                          \first two options belong to the spatiotemporal interpolation setup, \
                          \and the last to the arbitrary dimension interpolation setup. There all \
                          \variables require the prefix \"indep\" followed by any variable name, \
                          \e.g. \"C1\" and \"C2\"."
    <> OH.hardline <> s2d "> [depC1, depC2, ...]: Dependent variable position. All variables require \
                          \the prefix \"dep\" followed by any variable name, e.g. \"C1\" and \"C2\"."
    <> OH.hardline
    ))
    )

optParseInObsTempSamplesFile :: OP.Parser FilePath
optParseInObsTempSamplesFile = OP.strOption (
    OP.long "tempSampFile" <>
    OP.metavar "FILE" <>
    OP.helpDoc ( Just (
                      s2d "Path to a .tsv/.cbor file with random age permutations per observation \
                          \e.g. produced with currycarbon --samplesFile. If this is given, then the \
                          \temporal position of each sample is not read from the --obsFile, but looked \
                          \up in this table. The pairs must be ordered like and by --obsFile and then \
                          \include the same amount of age samples per observation, so that the table looks like this:"
    <> OH.hardline <>     "┌───────┬──────────┐"
    <> OH.hardline <>     "│ obsID │ yearBCAD │"
    <> OH.hardline <>     "├───────┼──────────┤"
    <> OH.hardline <>     "│     a │          │"
    <> OH.hardline <>     "│     a │          │"
    <> OH.hardline <>     "│     a │          │"
    <> OH.hardline <>     "│     b │          │"
    <> OH.hardline <>     "│     b │          │"
    <> OH.hardline <>     "│     b │          │"
    <> OH.hardline <>     "└───────┴──────────┘"
    <> OH.hardline <> s2d "> [obsID]: Observations identifier"
    <> OH.hardline <> s2d "> [yearBCAD]: Age sample"
    <> OH.hardline
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
        <$> OP.optional optParseSpaceTimeFilter
        <*> OP.optional optParseInSpatDistMapFile
        <*> OP.optional optParseInObsTempSamplesFile
        <*> optParseInSpatDistNoOrderCheck

optParseSpaceTimeFilter :: OP.Parser (Double,Double)
optParseSpaceTimeFilter = OP.option (OP.eitherReader readSpaceTime) (
       OP.long    "spaceTimeFilter"
    <> OP.metavar "filter(spatialRadius = DOUBLE, temporalRadius = DOUBLE)"
    <> OP.helpDoc ( Just (
                      s2d "Spatiotemporal radius filter to reduce the number of observations that \
                          \should be considered for each prediction grid point. This is primarily \
                          \a performance feature to speed up the calculation by ignoring far away \
                          \observations."
    <> OH.hardline
    ))
    )
    where
        readSpaceTime :: String -> Either String (Double, Double)
        readSpaceTime s =
            case P.runParser parseSpaceTime () "" s of
                Left err -> Left $ showParsecErr err
                Right x  -> Right x
        parseSpaceTime :: P.Parser (Double, Double)
        parseSpaceTime = do
            parseRecordType "filter" $ do
                a <- parseArgument "spatialRadius" parseDouble
                b <- parseArgument "temporalRadius" parseDouble
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
                          \and by --spatGridFile, so that the table looks like this:"
    <> OH.hardline <>     "┌───────┬────────┬──────┐"
    <> OH.hardline <>     "│ obsID │ spatID │ dist │"
    <> OH.hardline <>     "├───────┼────────┼──────┤"
    <> OH.hardline <>     "│     a │      x │      │"
    <> OH.hardline <>     "│     a │      y │      │"
    <> OH.hardline <>     "│     a │      z │      │"
    <> OH.hardline <>     "│     b │      x │      │"
    <> OH.hardline <>     "│     b │      y │      │"
    <> OH.hardline <>     "│     b │      z │      │"
    <> OH.hardline <>     "└───────┴────────┴──────┘"
    <> OH.hardline <> s2d "> [obsID]: Observations identifier"
    <> OH.hardline <> s2d "> [spatID]: Spatial coordinate identifier"
    <> OH.hardline <> s2d "> [dist]: Spatial distance"
    <> OH.hardline
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
    <> OH.hardline
    ))
    )

optParseInArbitraryDimFile :: OP.Parser FilePath
optParseInArbitraryDimFile = OP.strOption (
       OP.long    "anyGridFile"
    <> OP.metavar "FILE"
    <> OP.helpDoc ( Just (
                      s2d "Path to a .tsv/.cbor file with arbitrary dimension coordinates where interpolation \
                          \and search should be performed. Columns:"
    <> OH.hardline <>     "┌─────────┬─────────┐"
    <> OH.hardline <>     "│ indepC1 │ indepC2 │"
    <> OH.hardline <>     "├─────────┼─────────┤"
    <> OH.hardline <>     "│         │         │"
    <> OH.hardline <>     "│         │         │"
    <> OH.hardline <>     "└─────────┴─────────┘"
    <> OH.hardline <> s2d "> [indepC1, indepC2, ...]: Independent variable position"
    <> OH.hardline
    ))
    )

optParseInSpatGridFile :: OP.Parser FilePath
optParseInSpatGridFile = OP.strOption (
       OP.long    "spatGridFile"
    <> OP.short   'g'
    <> OP.metavar "FILE"
    <> OP.helpDoc ( Just (
                      s2d "Path to a .tsv/.cbor file with spatial coordinates where interpolation \
                          \and search should be performed. Columns:"
    <> OH.hardline <>     "┌────────┬───┬───┐"
    <> OH.hardline <>     "│ spatID │ x │ y │"
    <> OH.hardline <>     "├────────┼───┼───┤"
    <> OH.hardline <>     "│        │   │   │"
    <> OH.hardline <>     "│        │   │   │"
    <> OH.hardline <>     "└────────┴───┴───┘"
    <> OH.hardline <> s2d "> [spatID]: Spatial coordinate identifier"
    <> OH.hardline <> s2d "> [x, y] or [longitude, langitude]: Spatial coordinates"
    <> OH.hardline
    ))
    )

optParseTempGridString :: OP.Parser [Int]
optParseTempGridString = OP.option (OP.eitherReader readTempGridString) (
       OP.long    "tempGrid"
    <> OP.short   't'
    <> OP.metavar "YEAR|c(YEAR1,YEAR2,...)|START:STOP:BY"
    <> OP.helpDoc ( Just (
                      s2d "Temporal positions in years BC/AD where interpolation and search should \
                          \be performed. Negative integer numbers mark years BC, positive numbers years AD. \
                          \Can be given in three forms:"
    <> OH.hardline <> s2d "> YEAR: One year, e.g. \"-3000\" for 3000BC"
    <> OH.hardline <> s2d "> c(YEAR1,YEAR2,...): A list of years, e.g. \"c(-3000, 1000)\" for 3000BC \
                          \and 1000AD"
    <> OH.hardline <> s2d "> START:STOP:BY: A sequence  of years, e.g. \"-3000:1000:1000\" for 3000BC, \
                          \2000BC, 1000BC, 0AD and 1000AD"
    <> OH.hardline
    ))
    )
    where
        readTempGridString :: String -> Either String [Int]
        readTempGridString s =
            case P.runParser parseTempGridString () "" s of
                Left err -> Left $ showParsecErr err
                Right x  -> Right x
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
    <> OP.metavar "c(depX=DOUBLE,depY=c(DOUBLE,DOUBLE,...),depZ=START:STOP:BY,...)"
    <> OP.help    "Dependent variable positions that should be queried."
    <> OP.helpDoc ( Just (
                      s2d "Dependent variable positions that should be \"searched\" for, so for which \
                          \similarity probabilities in the interpolated field should be computed. \
                          \Each dependent variable must be specified in a named list \"c(depC1 = ..., depC2 = ..., ...)\". \
                          \And for each dependent variable either a single coordinate, a list of coordinates, \
                          \or a sequence of coordinates can be listed."
    <> OH.hardline
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
    <> OP.help    "Path to the .tsv/.cbor file with search observations."
    )

optParseOutFile :: OP.Parser FilePath
optParseOutFile = OP.strOption (
       OP.long  "outFile"
    <> OP.short 'o'
    <> OP.metavar "FILE"
    <> OP.help  "Path to the output file."
    )

optParseVariogramOutFile :: OP.Parser (Maybe FilePath)
optParseVariogramOutFile = OP.optional $ OP.strOption (
       OP.long  "variogramOutFile"
    <> OP.metavar "FILE"
    <> OP.help  "Path to the variogram output file."
    )

--optParseQuiet :: OP.Parser Bool
--optParseQuiet = OP.switch (
--    OP.long "quiet" <>
--    OP.short 'q' <>
--    OP.help "Suppress the printing of ..."
--    )

optParseKernDefString :: OP.Parser KernelDefinition
optParseKernDefString = OP.option (OP.eitherReader readKernDefString) (
       OP.long    "kerndef"
    <> OP.short   'k'
    <> OP.metavar "DSL"
    <> OP.help    "Kernel parameter settings that should be applied for the interpolation"
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
                    return $ KernelDefinition $ map (\(name,(s,n,l)) -> KernelOneDepVar name s n l) nested
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
    <> OP.help    "Kernel parameter settings that should be tested with the crossvalidation."
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

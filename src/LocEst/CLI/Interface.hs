{-# LANGUAGE FlexibleContexts #-}

module LocEst.CLI.Interface where

import           LocEst.CLI.ConfigLang
import           LocEst.CLI.Search
import           LocEst.Types

import           Control.Exception     (throw)
import           Data.Char             (isSpace, toLower)
import           Data.List             (groupBy, singleton)
import           LocEst.Utils
import qualified Options.Applicative   as OP
import qualified Text.Parsec           as P
import qualified Text.Parsec.String    as P
import           Text.Read             (readMaybe)

-- config file that uses the optparse interface

parseConfigFile :: FilePath -> IO [String]
parseConfigFile configFile = do
    contents <- readFile configFile
    case P.parse parseFile configFile contents of
        Left err -> throw $ ConfigFileParsingException $ show err
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
    <> OP.help    "Path to the .tsv file with input observations to inform the field."
    )

optParseInObsTempSamplesFile :: OP.Parser (Maybe FilePath)
optParseInObsTempSamplesFile = OP.option (Just <$> OP.str) (
    OP.long "tempSampFile" <>
    OP.metavar "FILE" <>
    OP.help "Path to file with the random age permutations per sample, \
            \e.g. produced with currycarbon." <>
    OP.value Nothing
    )

optParseSearchGridSettings :: OP.Parser SearchGridSettings
optParseSearchGridSettings =
    SearchGridSettings
        <$> optParseIndepVarsPredGridSettings
        <*> OP.optional optParseSearchDepVarsPos

optParseIndepVarsPredGridSettings :: OP.Parser IndepVarsPredGridSettings
optParseIndepVarsPredGridSettings =
    (SpaceTimeGridSettings
        <$> optParseInSpatGridFile
        <*> optParseTempGridString
        <*> optParseSpaceTimeFilter
        <*> OP.optional optParseInSpatDistMapFile
        <*> optParseInObsTempSamplesFile
    ) OP.<|>
    (ArbitraryDimGridSettings <$> optParseInArbitraryDimFile)

optParseSpaceTimeFilter :: OP.Parser (Maybe (Double,Double))
optParseSpaceTimeFilter = OP.option (Just <$> OP.eitherReader readSpaceTime) (
       OP.long    "spaceTimeFilter"
    <> OP.metavar "filter(spatialRadius = DOUBLE, temporalRadius = DOUBLE)"
    <> OP.help    "Filter list of relevant observations for each prediction point by space and time. \
                   \ This can be set to speed up the calculation."
    <> OP.value Nothing
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
                return $ (a, b)

optParseInSpatDistMapFile :: OP.Parser FilePath
optParseInSpatDistMapFile = OP.strOption (
       OP.long    "spatDistFile"
    <> OP.metavar "FILE"
    <> OP.help    "Path to a .tsv file with spatial distances between observations and the spatial \
                   \positions of interest. Must be ordered as --obsFile and --spatGridFile."
    )

optParseInSpatDistNoOrderCheck :: OP.Parser Bool
optParseInSpatDistNoOrderCheck = OP.switch (
    OP.long "noOrderCheck" <>
    OP.help "Don't validate the order of the spatDistFile and the tempSampFile to speed up the reading. \
             \Should only be set if the order is certainly correct."
    )

optParseInArbitraryDimFile :: OP.Parser FilePath
optParseInArbitraryDimFile = OP.strOption (
       OP.long    "anyGridFile"
    <> OP.metavar "FILE"
    <> OP.help    "Path to the .tsv file with the arbitrary dimension coordinates to be queried."
    )

optParseInSpatGridFile :: OP.Parser FilePath
optParseInSpatGridFile = OP.strOption (
       OP.long    "spatGridFile"
    <> OP.short   'g'
    <> OP.metavar "FILE"
    <> OP.help    "Path to the .tsv file with the spatial coordinates to be queried."
    )

optParseTempGridString :: OP.Parser [Int]
optParseTempGridString = OP.option (OP.eitherReader readTempGridString) (
       OP.long    "tempGrid"
    <> OP.short   't'
    <> OP.metavar "YEAR|c(YEAR1,YEAR2,...)|START:STOP:BY"
    <> OP.help    "Temporal positions that should be queried."
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
       OP.long    "depVars"
    <> OP.short   'd'
    <> OP.metavar "c(depX=DOUBLE,depY=c(DOUBLE,DOUBLE,...),depZ=START:STOP:BY,...)"
    <> OP.help    "Dependent variable positions that should be queried."
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
            return $ map DepVarsPos permutations
            where
                parseSequence = parseDoubleSequence
                parseList = parseVector parseDouble
                parseSingle = singleton <$> parseDouble

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

optParseInNrBins :: OP.Parser (Maybe Int)
optParseInNrBins = OP.optional $ OP.option OP.auto (
       OP.long  "nrBins"
    <> OP.short 'b'
    <> OP.metavar "INT"
    <> OP.help  "..."
    )

--optParseQuiet :: OP.Parser Bool
--optParseQuiet = OP.switch (
--    OP.long "quiet" <>
--    OP.short 'q' <>
--    OP.help "Suppress the printing of ..."
--    )

optParseAlgorithmString :: OP.Parser LocestAlgorithm
optParseAlgorithmString = OP.option (OP.eitherReader readAlgorithmString) (
       OP.long    "algorithm"
    <> OP.short   'a'
    <> OP.metavar "DSL"
    <> OP.help    "Algorithm that should be applied for the interpolation and search, including \
                   \ kernel parameter settings."
    )
    where
        readAlgorithmString :: String -> Either String LocestAlgorithm
        readAlgorithmString s =
            case P.runParser parseAlgorithmString () "" s of
                Left err -> Left $ showParsecErr err
                Right x  -> Right x
        parseAlgorithmString :: P.Parser LocestAlgorithm
        parseAlgorithmString = do
            P.try parseAlgoKernelSmooth -- P.<|> parseOtherAlgo
            where
                parseAlgoKernelSmooth = do
                    parseRecordType "kas" $ AlgoKernSmooth <$> parseArgument "kernels" parseKernelDef
                parseKernelDef = do
                    nested <- parseNamedVector parseDepVarName parseNuggetAndWidths
                    return $ KernelDefinition $ map (\(name,(nugget,l)) -> KernelOneDepVar name nugget (SquaredExponential $ ArbitraryDimPos l)) nested
                parseNuggetAndWidths = do
                    parseRecordType "depVar" $ do
                        a <- parseArgument "nugget" parseDouble
                        b <- parseArgument "kernelWidths" parseKernelWidths
                        return (a,b)
                parseKernelWidths = do
                    parseNamedVector parseIndepVarName parseDouble

-- general parsers

parseIndepVarName :: P.Parser String
parseIndepVarName =
          P.string "space"
    P.<|> P.string "time"
    P.<|> P.string "indep" <> P.many1 P.alphaNum

parseDepVarName :: P.Parser String
parseDepVarName = P.string "dep" <> P.many1 P.alphaNum

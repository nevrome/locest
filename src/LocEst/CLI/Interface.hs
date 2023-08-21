{-# LANGUAGE FlexibleContexts #-}

module LocEst.CLI.Interface where

import           LocEst.CLI.Crossvalidate
import           LocEst.CLI.Search
import           LocEst.Types

import           Data.Char                (isSpace)
import           Data.Function            ((&))
import qualified Data.HashMap.Strict      as HM
import           Data.List                (groupBy, singleton)
import qualified Options.Applicative      as OP
import qualified Text.Parsec              as P
import qualified Text.Parsec.Error        as P
import qualified Text.Parsec.String       as P

-- config file that uses the optparse interface

parseConfigFile :: FilePath -> IO [String]
parseConfigFile configFile = do
    contents <- readFile configFile
    let optparseInput = configFileToCLIInput contents
    --hPutStrLn stderr $ show optparseInput
    return optparseInput
    where
    configFileToCLIInput :: String -> [String]
    configFileToCLIInput conf =
        lines conf &
        map removeComments &
        filter (not . null) &
        concatMap splitOnFirstColon
    removeComments :: String -> String
    removeComments = takeWhile (/= '#')
    splitOnFirstColon :: String -> [String]
    splitOnFirstColon s = case break (==':') s of (a,b) -> [dash (trim a), trim (tail b)]
    trim :: String -> String
    trim = let f = reverse . dropWhile isSpace in f . f
    dash :: String -> String
    dash s
      | length s == 1 = '-'  :  s
      | otherwise     = "--" ++ s

-- optparse-applicative interface

optParseInObservationFile :: OP.Parser FilePath
optParseInObservationFile = OP.strOption (
       OP.long    "obsFile"
    <> OP.short   'i'
    <> OP.metavar "FILE"
    <> OP.help    "Path to the .tsv file with input observations to inform the field."
    )

optParseConcretePositionSettings :: OP.Parser ConcretePositionSettings
optParseConcretePositionSettings =
    ConcretePositionSettings
        <$> optParseInSpatGridFile
        <*> optParseTempGridString
        <*> optParseSearchDepVarsPos
        <*> optParseInSpatDistMapFile

optParseCrossvalidationSettings :: OP.Parser CrossvalidationSettings
optParseCrossvalidationSettings =
    CrossvalidationSettings
        <$> optParseTestTrainingFraction
        <*> optParseCrossvalIterations

optParseTestTrainingFraction :: OP.Parser Double
optParseTestTrainingFraction = OP.option (OP.eitherReader readFraction) (
       OP.long    "testFraction"
    <> OP.metavar "..."
    <> OP.help    "..."
    )

optParseCrossvalIterations :: OP.Parser Int
optParseCrossvalIterations = OP.option OP.auto (
       OP.long    "iterations"
    <> OP.metavar "..."
    <> OP.help    "..."
    )

readFraction :: String -> Either String Double
readFraction s =
    case P.runParser parseFraction () "" s of
        Left err -> Left $ showParsecErr err
        Right x  -> Right x

optParseInSpatDistMapFile :: OP.Parser (Maybe FilePath)
optParseInSpatDistMapFile = OP.option (Just <$> OP.str) (
       OP.long    "spatDistFile"
    <> OP.metavar "FILE"
    <> OP.help    "Path to a .tsv file with spatial distances between observations and the spatial \
                   \positions of interest."
    <> OP.value Nothing
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
    <> OP.metavar "[YEAR|c(YEAR1,YEAR2,...)]"
    <> OP.help    "Temporal positions that should be queried."
    )

readTempGridString :: String -> Either String [Int]
readTempGridString s =
    case P.runParser parseTempGridString () "" s of
        Left err -> Left $ showParsecErr err
        Right x  -> Right x

parseTempGridString :: P.Parser [Int]
parseTempGridString = do
    P.try parseIntegerSequence P.<|> parseYearList
    where 
        parseYearList = parseVector parseInteger

optParseSearchDepVarsPos :: OP.Parser [DepVarsPos]
optParseSearchDepVarsPos = OP.option (OP.eitherReader readSearchDepVarsPos) (
       OP.long    "depVars"
    <> OP.short   'd'
    <> OP.metavar "c(varX=DOUBLE,[varY=DOUBLE|varY=START:STOP:BY],...)"
    <> OP.help    "Dependent variable positions that should be queried."
    )

readSearchDepVarsPos :: String -> Either String [DepVarsPos]
readSearchDepVarsPos s =
    case P.runParser parseSearchDepVarsPos () "" s of
        Left err -> Left $ showParsecErr err
        Right x  -> Right x

parseSearchDepVarsPos :: P.Parser [DepVarsPos]
parseSearchDepVarsPos = do
    res <- parseNamedVector parseVarName (P.try parseDoubleSequence P.<|> (singleton <$> parseDouble))
    let flattened = concatMap (\(str, dblList) -> map (\dbl -> (str, dbl)) dblList) res
        grouped = groupBy (\(str1, _) (str2, _) -> str1 == str2) flattened
        permutations = sequenceA grouped
    return $ map (DepVarsPos . HM.fromList) permutations

optParseOutFile :: OP.Parser FilePath
optParseOutFile = OP.strOption (
       OP.long  "outFile"
    <> OP.short 'o'
    <> OP.metavar "FILE"
    <> OP.help  "Path to the output file."
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

readAlgorithmString :: String -> Either String LocestAlgorithm
readAlgorithmString s =
    case P.runParser parseAlgorithmString () "" s of
        Left err -> Left $ showParsecErr err
        Right x  -> Right x

parseAlgorithmString :: P.Parser LocestAlgorithm
parseAlgorithmString = do
    P.try parseAlgoSepIDW P.<|> parseAlgoKernelSmooth
    where
        parseAlgoSepIDW = do
            _ <- P.string "SepIDW("
            decayDef <- parseDecayDef
            consumeCommaSep
            sumAlg <- parseSumAlg
            _ <- P.char ')'
            return $ AlgoSepIDW decayDef sumAlg
            where
                parseDecayDef = do
                    decayVec <- parseNamedVector parseVarName parseDecayAlgorithm
                    return $ DecayDefinition $ map (uncurry DecayOneDepVar) decayVec
                parseDecayAlgorithm = P.try parseLinearSum P.<|> parseLogSum
                parseLinearSum = do
                  _ <- P.string "LinearSum"
                  _ <- P.char '('
                  _ <- P.spaces
                  a <- parseDouble
                  consumeCommaSep
                  b <- parseDouble
                  _ <- P.char ')'
                  return $ LinearSum a b
                parseLogSum = do
                  _ <- P.string "LogSum"
                  _ <- P.char '('
                  _ <- P.spaces
                  a <- parseDouble
                  consumeCommaSep
                  b <- parseDouble
                  _ <- P.char ')'
                  return $ LogSum a b
                parseSumAlg = P.try parseMaximum P.<|> parseMean P.<|> parseDistanceWeightedMean
                parseMaximum =
                    P.string "Maximum" >> return Maximum
                parseMean =
                    P.string "Mean" >> return Mean
                parseDistanceWeightedMean =
                    P.string "DistanceWeightedMean" >> return DistanceWeightedMean
        parseAlgoKernelSmooth = do
            _ <- P.string "KernSmooth("
            kernDef <- parseKernelDef
            _ <- P.char ')'
            return $ AlgoKernSmooth kernDef
            where
                parseKernelDef = do
                    kernelVec <- parseNamedVector parseVarName parseKernel
                    return $ KernelDefinition $ map (uncurry KernelOneDepVar) kernelVec
                parseKernel = P.try parseUniform P.<|> parseNormal
                parseUniform = do
                  _ <- P.string "Uniform"
                  _ <- P.char '('
                  _ <- P.spaces
                  spat <- parseDouble
                  consumeCommaSep
                  temp <- parseDouble
                  _ <- P.spaces
                  _ <- P.char ')'
                  return $ Uniform spat temp
                parseNormal = do
                  _ <- P.string "Normal"
                  _ <- P.char '('
                  _ <- P.spaces
                  spat <- parseDouble
                  consumeCommaSep
                  temp <- parseDouble
                  _ <- P.spaces
                  _ <- P.char ')'
                  return $ Normal spat temp

-- general parsers

parseVarName :: P.Parser String
parseVarName = P.string "var" <> P.many1 P.alphaNum

parseNamedVector :: P.Parser a -> P.Parser b -> P.Parser [(a,b)]
parseNamedVector parseKey parseValue =
    parseVector $ parseKeyValuePair parseKey parseValue

parseKeyValuePair :: P.Parser a -> P.Parser b -> P.Parser (a,b)
parseKeyValuePair parseKey parseValue = do
    key <- parseKey
    consumeEqualSep
    value <- parseValue
    return (key, value)

parseVector :: P.Parser a -> P.Parser [a]
parseVector parseValue = do
    _ <- P.string "c"
    _ <- P.char '('
    _ <- P.spaces
    res <- P.sepBy parseValue consumeCommaSep
    _ <- P.spaces
    _ <- P.char ')'
    return res

consumeEqualSep :: P.Parser ()
consumeEqualSep = do
    _ <- P.spaces *> P.char '=' <* P.spaces
    return ()
consumeCommaSep :: P.Parser ()
consumeCommaSep = do
    _ <- P.spaces *> P.char ',' <* P.spaces
    return ()

parseDoubleSequence :: P.Parser [Double]
parseDoubleSequence = do
    start <- parseDouble
    _ <- P.oneOf ":"
    stop <- parseDouble
    _ <- P.oneOf ":"
    by <- parsePositiveFloatNumber
    return [start,(start+by)..stop]

parseDouble :: P.Parser Double
parseDouble = do
    P.try parseNegativeFloatNumber P.<|> parsePositiveFloatNumber

parseNegativeFloatNumber :: P.Parser Double
parseNegativeFloatNumber = do
    _ <- P.oneOf "-"
    i <- parsePositiveFloatNumber
    return (-i)

parseFraction :: P.Parser Double
parseFraction = do
    num <- parsePositiveFloatNumber
    if num > 1
    then fail "must be between zero and one"
    else return num

parsePositiveFloatNumber :: P.Parser Double
parsePositiveFloatNumber = do
    num <- parseNumber
    optionalMore <- P.option "" $ (:) <$> P.char '.' <*> parseNumber
    return $ read $ num ++ optionalMore

parseIntegerSequence :: P.Parser [Int]
parseIntegerSequence = do
    start <- parseInteger
    _ <- P.oneOf ":"
    stop <- parseInteger
    _ <- P.oneOf ":"
    by <- parsePositiveInteger
    return [start,(start+by)..stop]

parseInteger :: P.Parser Int
parseInteger = do
    P.try parseNegativeInteger P.<|> parsePositiveInteger

parseNegativeInteger :: P.Parser Int
parseNegativeInteger = do
    _ <- P.oneOf "-"
    i <- parsePositiveInteger
    return (-i)

parsePositiveInteger :: P.Parser Int
parsePositiveInteger = do
    read <$> parseNumber

parseNumber :: P.Parser [Char]
parseNumber = P.many1 P.digit

-- helper functions

showParsecErr :: P.ParseError -> String
showParsecErr err =
    P.showErrorMessages
        "or" "unknown parse error"
        "expecting" "unexpected" "end of input"
        (P.errorMessages err)
module LocEst.CLI.Interface where

import           LocEst.Types
import           LocEst.CLI.Search
import           LocEst.CLI.Crossvalidate

import qualified Data.HashMap.Strict as HM
import qualified Options.Applicative as OP
import qualified Text.Parsec         as P
import qualified Text.Parsec.Error  as P
import qualified Text.Parsec.String  as P
import Data.Char (isSpace)
import Data.Function ((&))
import System.IO (hPutStrLn, stderr)
import Text.Parsec.Error (errorMessages)

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

-- general parsers

parseDoubleSequence = do
    start <- parseDouble
    _ <- P.oneOf ":"
    stop <- parseDouble
    _ <- P.oneOf ":"
    by <- parsePositiveFloatNumber
    return [start,(start+by)..stop]

parseDouble = do
    P.try parseNegativeFloatNumber P.<|> parsePositiveFloatNumber

parseNegativeFloatNumber = do
    _ <- P.oneOf "-"
    i <- parsePositiveFloatNumber
    return (-i)

parseFraction = do
    num <- parsePositiveFloatNumber
    if num > 1
    then fail "must be between zero and one"
    else return num

parsePositiveFloatNumber = do
    num <- parseNumber
    optionalMore <- P.option "" $ (:) <$> P.char '.' <*> parseNumber
    return $ read $ num ++ optionalMore

parseIntegerSequence = do
    start <- parseInteger
    _ <- P.oneOf ":"
    stop <- parseInteger
    _ <- P.oneOf ":"
    by <- parsePositiveInteger
    return [start,(start+by)..stop]

parseInteger = do
    P.try parseNegativeInteger P.<|> parsePositiveInteger

parseNegativeInteger = do
    _ <- P.oneOf "-"
    i <- parsePositiveInteger
    return (-i)

parsePositiveInteger = do
    read <$> parseNumber

parseNumber = P.many1 P.digit

-- optparse definitions

optParseInObservationFile :: OP.Parser FilePath
optParseInObservationFile = OP.strOption (
       OP.long    "obsFile"
    <> OP.short   'i'
    <> OP.metavar "FILE"
    <> OP.help    "..."
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
    <> OP.help    "..."
    <> OP.value Nothing
    )

optParseInSpatGridFile :: OP.Parser FilePath
optParseInSpatGridFile = OP.strOption (
       OP.long    "spatGridFile"
    <> OP.short   'g'
    <> OP.metavar "FILE"
    <> OP.help    "..."
    )

optParseTempGridString :: OP.Parser [Int]
optParseTempGridString = OP.option (OP.eitherReader readTempGridString) (
       OP.long    "tempGrid"
    <> OP.short   't'
    <> OP.metavar "YEAR|YEAR1,YEAR2,...|START:STOP:BY"
    <> OP.help    "..."
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
        parseYearList = do
            P.sepBy parseInteger (P.char ',' <* P.spaces) <* P.eof

optParseSearchDepVarsPos :: OP.Parser [DepVarsPos]
optParseSearchDepVarsPos = OP.option (OP.eitherReader readSearchDepVarsPos) (
       OP.long    "depVars"
    <> OP.short   'd'
    <> OP.metavar "varX=DOUBLE+varY=DOUBLE,..."
    <> OP.help    "..."
    )

readSearchDepVarsPos :: String -> Either String [DepVarsPos]
readSearchDepVarsPos s =
    case P.runParser parseSearchDepVarsPos () "" s of
        Left err -> Left $ showParsecErr err
        Right x  -> Right x

parseSearchDepVarsPos :: P.Parser [DepVarsPos]
parseSearchDepVarsPos =
        P.try parseSearchDepVarsPosGrid P.<|> parseSearchDepVarsPosSimpleList
    where

    parseSearchDepVarsPosGrid :: P.Parser [DepVarsPos]
    parseSearchDepVarsPosGrid = do
        listOfSequencesPerVar <- P.sepBy parseSearchDepVarsPosGridOneSequence (P.char '+' <* P.spaces) <* P.eof
        -- create all permutations
        let combinations = sequenceA listOfSequencesPerVar
        return $ map (DepVarsPos . HM.fromList) combinations
    parseSearchDepVarsPosGridOneSequence :: P.Parser [(String, Double)]
    parseSearchDepVarsPosGridOneSequence = do
        identifier <- P.string "var" <> P.many1 P.alphaNum
        P.spaces
        _ <- P.char '='
        P.spaces
        doubleSequence <- parseDoubleSequence
        return $ map (\x -> (identifier, x)) doubleSequence

    parseSearchDepVarsPosSimpleList :: P.Parser [DepVarsPos]
    parseSearchDepVarsPosSimpleList = do
        P.sepBy parseSearchDepVarsPosSimpleListOne (P.char ',' <* P.spaces) <* P.eof
    parseSearchDepVarsPosSimpleListOne :: P.Parser DepVarsPos
    parseSearchDepVarsPosSimpleListOne = do
        resList <- P.sepBy parseDepVarCoord (P.spaces *> P.char '+' <* P.spaces)
        return $ DepVarsPos $ HM.fromList resList
    parseDepVarCoord = do
        identifier <- P.string "var" <> P.many1 P.alphaNum
        P.spaces
        _ <- P.char '='
        P.spaces
        number <- parseDouble
        return (identifier, number)

optParseOutFile :: OP.Parser FilePath
optParseOutFile = OP.strOption (
       OP.long  "outFile"
    <> OP.short 'o'
    <> OP.metavar "FILE"
    <> OP.help  "..."
    )

--optParseQuiet :: OP.Parser Bool
--optParseQuiet = OP.switch (
--    OP.long "quiet" <>
--    OP.short 'q' <>
--    OP.help "Suppress the printing of ..."
--    )

-- helper functions

showParsecErr :: P.ParseError -> String
showParsecErr err =
    P.showErrorMessages
        "or" "unknown parse error"
        "expecting" "unexpected" "end of input"
        (P.errorMessages err)
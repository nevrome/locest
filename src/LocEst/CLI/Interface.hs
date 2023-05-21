module LocEst.CLI.Interface where

import           LocEst.Types

import qualified Data.HashMap.Strict as HM
import qualified Options.Applicative as OP
import qualified Text.Parsec         as P
import qualified Text.Parsec.String  as P

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
        Left err -> Left $ show err
        Right x  -> Right x

parseTempGridString :: P.Parser [Int]
parseTempGridString = do
    P.try parseIntegerSequence P.<|> parseYearList
    where
        parseYearList = do
            P.sepBy parseInteger (P.char ',' <* P.spaces) <* P.eof

optParseSearchDepVars :: OP.Parser DepVarsPos
optParseSearchDepVars = OP.option (OP.eitherReader readSearchDepVars) (
       OP.long    "depVars"
    <> OP.short   'd'
    <> OP.metavar "varX:DOUBLE,varY:DOUBLE,..."
    <> OP.help    "..."
    )

readSearchDepVars :: String -> Either String DepVarsPos
readSearchDepVars s =
    case P.runParser parseSearchDepVars () "" s of
        Left err -> Left $ show err
        Right x  -> Right x

parseSearchDepVars :: P.Parser DepVarsPos
parseSearchDepVars = do
    resList <- P.sepBy parseDepVarCoord (P.char ',' <* P.spaces) <* P.eof
    return $ DepVarsPos $ HM.fromList resList
    where
        parseDepVarCoord = do
            identifier <- P.string "var" <> P.many1 P.alphaNum
            _ <- P.char ':'
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

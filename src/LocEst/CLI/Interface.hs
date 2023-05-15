module LocEst.CLI.Interface where

import qualified Options.Applicative as OP
import qualified Text.Parsec         as P
import qualified Text.Parsec.String  as P

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
    P.try parseSeq P.<|> parseYearList
    where
        parseYearList = do
            P.sepBy parseInteger (P.char ',' <* P.spaces) <* P.eof
        parseSeq = do
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
            i <- read <$> P.many1 P.digit
            return i

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

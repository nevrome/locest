module LocEst.CLI.Interface where

import qualified Options.Applicative                as OP

parseInObservationFile :: OP.Parser FilePath
parseInObservationFile = OP.strOption (
       OP.long "obsFile"
    <> OP.short 'i'
    <> OP.help "..."
    )

parseInSpatGridFile :: OP.Parser FilePath
parseInSpatGridFile = OP.strOption (
       OP.long  "spatGridFile"
    <> OP.short 'g'
    <> OP.help  "..."
    )

parseOutFile :: OP.Parser FilePath
parseOutFile = OP.strOption (
       OP.long  "outFile"
    <> OP.short 'o'
    <> OP.help  "..."
    )

--optParseQuiet :: OP.Parser Bool
--optParseQuiet = OP.switch (
--    OP.long "quiet" <> 
--    OP.short 'q' <>
--    OP.help "Suppress the printing of ..."
--    )
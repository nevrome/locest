{-# LANGUAGE FlexibleContexts #-}

module LocEst.Parsers where

import           LocEst.Types

import           Conduit                   (MonadIO, MonadResource, liftIO)
import           Control.Exception         (throwIO)
import           Control.Monad             (when)
import           Control.Monad.Error.Class
import qualified Data.ByteString.Builder   as BB
import qualified Data.ByteString.Char8     as Bchs
import           Data.Char                 (ord)
import           Data.Conduit              (ConduitT, Void, (.|))
import qualified Data.Conduit              as Con
import qualified Data.Conduit.Combinators  as ConC
import qualified Data.Conduit.List         as ConL
import qualified Data.Csv                  as Csv
import qualified Data.Csv.Builder          as CsvB
import qualified Data.Csv.Conduit          as ConCsv
import           Data.IORef                (modifyIORef, newIORef, readIORef)
import           LocEst.Utils              (LOCESTException (NormalException))
import           System.IO                 (Handle, IOMode (..), hClose,
                                            hPutStrLn, openFile, stderr)

-- helper functions
decodingOptions :: Csv.DecodeOptions
decodingOptions = Csv.defaultDecodeOptions {
    Csv.decDelimiter = fromIntegral (ord '\t')
}

encodingOptions :: Csv.EncodeOptions
encodingOptions = Csv.defaultEncodeOptions {
      Csv.encDelimiter = fromIntegral (ord '\t')
    }

readSpatDist :: FilePath -> IO SpatDistMap
readSpatDist path = do
    obsGridDists <- readCSVToList path
    return $ makeSpatDistMap obsGridDists
readObservations :: FilePath -> IO [Observation]
readObservations = readCSVToList
readSpatTempDepVarsPos :: FilePath -> IO [SpatTempDepVarsPos]
readSpatTempDepVarsPos = readCSVToList
readSpatPos :: FilePath -> IO [SpatPos]
readSpatPos = readCSVToList

readCSVToList :: (Csv.FromNamedRecord a) => FilePath -> IO [a]
readCSVToList path = do
    hPutStrLn stderr $ "Parsing " ++ path
    parseRes <- Con.runConduitRes $ sourceCSV path .| ConL.consume
    goodData <- mapM unwrapCSVParsingErrors parseRes
    hPutStrLn stderr "Done"
    return goodData
    where
        unwrapCSVParsingErrors :: (Show b, Show c) => Either (Either b c) a -> IO a
        unwrapCSVParsingErrors parseRes =
            case parseRes of
                Left e ->
                    case e of
                        Left e1  -> liftIO $ throwIO $ NormalException $ show e1
                        Right e2 -> liftIO $ throwIO $ NormalException $ show e2
                Right res -> return res

sourceCSV :: (MonadResource m, MonadError IOError m, Csv.FromNamedRecord a) =>
                FilePath
             -> ConduitT () (Either (Either ConCsv.CsvStreamHaltParseError ConCsv.CsvStreamRecordParseError) a) m ()
sourceCSV path =
       ConC.sourceFile path
    .| ConCsv.fromNamedCsvStreamErrorNoThrow decodingOptions

sinkNamedCSV :: (MonadResource m, Csv.ToRecord a, Csv.DefaultOrdered a) => FilePath -> ConduitT a Void m ()
sinkNamedCSV path =
    Con.bracketP (openFile path WriteMode) hClose $ \handle ->
           writeHeaderCSV handle
        .| ConCsv.toCsv encodingOptions
        .| ConL.mapM_ (liftIO . Bchs.hPutStr handle)
    where
        writeHeaderCSV :: (MonadIO m, Csv.DefaultOrdered i) => Handle -> ConduitT i i m ()
        writeHeaderCSV handle = do
            flagRef <- liftIO $ newIORef True
            ConL.mapM $ \val -> do
                -- run the action only for the first element
                flag <- liftIO $ readIORef flagRef
                when flag $ do
                    liftIO $ BB.hPutBuilder handle $ CsvB.encodeHeaderWith encodingOptions $ Csv.headerOrder val
                    liftIO $ modifyIORef flagRef not
                -- continue processing the rest of the stream
                return val

sinkCSV :: (MonadResource m, Csv.ToRecord a) => FilePath -> ConduitT a Void m ()
sinkCSV path =
       ConCsv.toCsv encodingOptions
    .| ConC.sinkFile path

progress :: (MonadIO m) => ConduitT i i m ()
progress = do
    counterRef <- liftIO $ newIORef (0 :: Int)
    ConL.mapM $ \val -> do
        n <- liftIO $ readIORef counterRef
        liftIO $ logProgress n
        liftIO $ modifyIORef counterRef (+1)
        return val
    where
        logProgress :: Int -> IO ()
        logProgress c
            |  c `rem` 1000 == 0 = hPutStrLn stderr $ "Iterations done: " ++ padLeft 7 (show c)
            -- |  c == 100          = putStrLn $ "Probing successful. Continuing now..."
            | otherwise = return ()

padLeft :: Int -> String -> String
padLeft n s
    | length s >= n = reverse (take n (reverse s))
    | length s < n = replicate (n - length s) ' ' ++ s
    | otherwise    = s

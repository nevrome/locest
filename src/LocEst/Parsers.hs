{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Strict           #-}

module LocEst.Parsers where

import           LocEst.Types

import           Conduit                   (MonadIO, MonadResource, liftIO)
import           Control.Exception         (throwIO)
import           Control.Monad             (when)
import           Control.Monad.Error.Class
--import qualified Control.Monad.State       as ST
import qualified Data.ByteString.Builder   as BB
import qualified Data.ByteString.Char8     as Bchs
import           Data.Char                 (ord)
import           Data.Conduit              (ConduitT, Void, (.|))
import qualified Data.Conduit              as Con
import qualified Data.Conduit.Combinators  as ConC
--import qualified Data.Conduit.Lift         as ConLF
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

readSpatDist :: Bool -> [Observation] -> [SpatPos] -> FilePath -> IO SpatDistMatrix
readSpatDist noOrderCheck obs spatGrid path = do
    hPutStrLn stderr $ "Parsing " ++ path
    let nObs = length obs
        nGridPoints = length spatGrid
    distVec <- Con.runConduitRes $
        sourceCSV path .|
        ConC.mapM unwrapCSVParsingErrors .|
        (
            if noOrderCheck
            then ConC.map (\(SpatDistObsGrid _ _ d) -> d)
            else checkOrder
        ) .|
        ConC.sinkVectorN (nObs * nGridPoints)
    hPutStrLn stderr "Done"
    return $ SpatDistMatrix nGridPoints nObs distVec
    where
    checkOrder :: (MonadIO m) => ConduitT SpatDistObsGrid Double m ()
    checkOrder = do
        let outerCycle = map getID obs
            innerCycle = map getID spatGrid
            fullCycle  = [(o,i) | o <- outerCycle, i <- innerCycle]
        loop fullCycle
        where
            loop (expected:rest) = do
                val <- Con.await
                case val of
                    Just oneSpatDist -> do
                        let (SpatDistObsGrid obsID spatID dist) = oneSpatDist
                        if (obsID, spatID) == expected
                        then do
                            Con.yield dist
                            loop rest
                        else do
                            -- throw an exception if the order is not as expected
                            liftIO $ throwIO $ NormalException $
                                "Order of entries in --spatDistFile not equal to -i and -g. " ++
                                "Expected: " ++ show (obsID, spatID) ++ " but got: " ++ show expected
                    Nothing -> return ()
            loop [] = return ()

readObservations :: FilePath -> IO [Observation]
readObservations = readCSVToList
readSpatTempDepVarsPos :: FilePath -> IO [SpatTempDepVarsPos]
readSpatTempDepVarsPos = readCSVToList
readSpatPos :: FilePath -> IO [SpatPos]
readSpatPos = readCSVToList

readCSVToList :: (Csv.FromNamedRecord a) => FilePath -> IO [a]
readCSVToList path = do
    hPutStrLn stderr $ "Parsing " ++ path
    parseRes <- Con.runConduitRes $ sourceCSV path .| ConC.mapM unwrapCSVParsingErrors .| ConL.consume
    hPutStrLn stderr "Done"
    return parseRes

unwrapCSVParsingErrors :: (Show b, Show c, MonadIO m) => Either (Either b c) a -> m a
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
    .| progress 1000000

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

progress :: (MonadIO m) => Int -> ConduitT i i m ()
progress reportNum = do
    counterRef <- liftIO $ newIORef (0 :: Int)
    ConL.mapM $ \val -> do
        n <- liftIO $ readIORef counterRef
        liftIO $ logProgress n
        liftIO $ modifyIORef counterRef (+1)
        return val
    where
        logProgress :: Int -> IO ()
        logProgress c
            |  c /= 0 && c `rem` reportNum == 0 = hPutStrLn stderr $ "Iterations done: " ++ padLeft 9 (show c)
            | otherwise = return ()

padLeft :: Int -> String -> String
padLeft n s
    | length s >= n = reverse (take n (reverse s))
    | length s < n = replicate (n - length s) ' ' ++ s
    | otherwise    = s

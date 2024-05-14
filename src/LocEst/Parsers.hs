{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Strict           #-}

module LocEst.Parsers where

import           LocEst.CLI.Utils
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
import qualified Data.Csv                  as Csv
import qualified Data.Csv.Builder          as CsvB
import qualified Data.Csv.Conduit          as ConCsv
import           Data.IORef                (modifyIORef, newIORef, readIORef)
import           LocEst.Utils              (LOCESTException (NormalException))
import           System.IO                 (Handle, IOMode (..), hClose,
                                            hPutStrLn, openFile, stderr)
import qualified Data.Vector as V

-- helper functions
decodingOptions :: Csv.DecodeOptions
decodingOptions = Csv.defaultDecodeOptions {
    Csv.decDelimiter = fromIntegral (ord '\t')
}

encodingOptions :: Csv.EncodeOptions
encodingOptions = Csv.defaultEncodeOptions {
      Csv.encDelimiter = fromIntegral (ord '\t')
    }

readTempSamp :: Bool -> V.Vector Observation -> FilePath -> IO TempSampleMatrix
readTempSamp noOrderCheck obs path = do
    hPutStrLn stderr $ "Parsing " ++ path
    let nObs = length obs
    -- determine number of samples to expect
    hPutStrLn stderr "Counting the number of age samples"
    nSamples <- Con.runConduitRes $
        sourceCSV path .|
        ConC.mapM unwrapCSVParsingErrors .|
        ConC.takeWhile (\(TempSample obsID _) -> obsID == _obsID (V.head obs)) .|
        ConC.length
    if nSamples > 0
    then hPutStrLn stderr $ "Expected age samples per observation: " ++ show nSamples
    else liftIO $ throwIO $ NormalException $
        "Order of entries in --tempSampFile not equal to -i. " ++
        "Expected first value: " ++ _obsID (V.head obs)
    -- start the actual parsing
    sampleVec <- Con.runConduitRes $
        sourceCSV path .|
        ConC.mapM unwrapCSVParsingErrors .|
        (
            if noOrderCheck
            then ConC.map (\(TempSample _ age) -> age)
            else checkOrder nSamples
        ) .|
        ConC.sinkVectorN (nObs * nSamples)
    hPutStrLn stderr "Done"
    return $ TempSampleMatrix nSamples nObs sampleVec
    where
    checkOrder :: (MonadIO m) => Int -> ConduitT TempSample YearBCAD m ()
    checkOrder nSamples = do
        loop (concatMap (replicate nSamples . getID) obs)
        where
            loop (expected:rest) = do
                val <- Con.await
                case val of
                    Just oneTempSamp -> do
                        let (TempSample obsID age) = oneTempSamp
                        if obsID == expected
                        then do
                            Con.yield age
                            loop rest
                        else do
                            -- throw an exception if the order is not as expected
                            liftIO $ throwIO $ NormalException $
                                "Order of entries in --tempSampFile not equal to -i. " ++
                                "Expected: " ++ expected ++ " but got: " ++ obsID
                    Nothing -> return ()
            loop [] = return ()

readSpatDist :: Bool -> V.Vector Observation -> V.Vector SpatPos -> FilePath -> IO SpatDistMatrix
readSpatDist noOrderCheck obs spatGrid path = do
    hPutStrLn stderr $ "Parsing " ++ path
    let nObs = V.length obs
        nGridPoints = length spatGrid
    distVec <- Con.runConduitRes $
        sourceCSV path .|
        ConC.mapM unwrapCSVParsingErrors .|
        (
            if noOrderCheck
            then ConC.map (\(SpatDistObsGrid _ _ dist) -> dist)
            else checkOrder
        ) .|
        ConC.sinkVectorN (nObs * nGridPoints)
    hPutStrLn stderr "Done"
    return $ AUDistMatrix nGridPoints nObs distVec
    where
    checkOrder :: (MonadIO m) => ConduitT SpatDistObsGrid Double m ()
    checkOrder = do
        let outerCycle = V.map getID obs
            innerCycle = V.map getID spatGrid
            fullCycle  = [(o,i) | o <- V.toList outerCycle, i <- V.toList innerCycle]
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
                                "Expected: " ++ show expected ++ " but got: " ++ show (obsID, spatID)
                    Nothing -> return ()
            loop [] = return ()

readObservations :: FilePath -> IO (V.Vector Observation)
readObservations = readCSVToVector
readHyperPos :: FilePath -> IO (V.Vector HyperPos)
readHyperPos = readCSVToVector
readArbitraryDimPos :: FilePath -> IO (V.Vector ArbitraryDimPos)
readArbitraryDimPos = readCSVToVector
readSpatPos :: FilePath -> IO (V.Vector SpatPos)
readSpatPos = readCSVToVector

readCSVToVector :: (Csv.FromNamedRecord a) => FilePath -> IO (V.Vector a)
readCSVToVector path = do
    hPutStrLn stderr $ "Parsing " ++ path
    parseRes <- Con.runConduitRes $ sourceCSV path .| ConC.mapM unwrapCSVParsingErrors .| ConC.sinkVector
    hPutStrLn stderr "Done"
    return parseRes

readCSVToList :: (Csv.FromNamedRecord a) => FilePath -> IO [a]
readCSVToList path = do
    hPutStrLn stderr $ "Parsing " ++ path
    parseRes <- Con.runConduitRes $ sourceCSV path .| ConC.mapM unwrapCSVParsingErrors .| ConC.sinkList
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
    .| progress 1000000 Nothing

sinkNamedCSV :: (MonadResource m, Csv.ToRecord a, Csv.DefaultOrdered a) => FilePath -> ConduitT a Void m ()
sinkNamedCSV path =
    Con.bracketP (openFile path WriteMode) hClose $ \handle ->
           writeHeaderCSV handle
        .| ConCsv.toCsv encodingOptions
        .| ConC.mapM_ (liftIO . Bchs.hPutStr handle)
    where
        writeHeaderCSV :: (MonadIO m, Csv.DefaultOrdered i) => Handle -> ConduitT i i m ()
        writeHeaderCSV handle = do
            flagRef <- liftIO $ newIORef True
            ConC.mapM $ \val -> do
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

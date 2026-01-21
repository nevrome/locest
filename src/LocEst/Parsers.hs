{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

module LocEst.Parsers where

import           LocEst.Types
import           LocEst.TypesFlat
import           LocEst.Utils

import qualified Codec.Serialise                as S
import           Conduit                        (MonadIO, MonadResource, liftIO)
import           Control.Monad                  (forM_, when)
import           Control.Monad.Error.Class
import qualified Data.ByteString.Builder        as BB
import qualified Data.ByteString.Char8          as Bchs
import qualified Data.ByteString.Lex.Fractional as LexFrac
import           Data.Char                      (ord)
import           Data.Conduit                   (ConduitT, Void, (.|))
import qualified Data.Conduit                   as Con
import qualified Data.Conduit.Combinators       as ConC
import qualified Data.Csv                       as Csv
import qualified Data.Csv.Builder               as CsvB
import qualified Data.Csv.Conduit               as ConCsv
import           Data.IORef                     (modifyIORef, newIORef,
                                                 readIORef)
import qualified Data.Vector                    as V
import qualified Data.Vector.Storable           as VS
import qualified Data.Vector.Storable.Mutable   as VSM
import           System.FilePath                (takeExtension)
import           System.IO                      (Handle, IOMode (..), hClose,
                                                 hPutStrLn, openFile, stderr,
                                                 stdout)

-- helper functions
decodingOptions :: Csv.DecodeOptions
decodingOptions = Csv.defaultDecodeOptions {
        Csv.decDelimiter = fromIntegral (ord '\t')
    }

encodingOptions :: Csv.EncodeOptions
encodingOptions = Csv.defaultEncodeOptions {
        Csv.encDelimiter = fromIntegral (ord '\t')
    }


-- matrix parsers

readSUDistMulti :: Int -> FilePath -> IO SUDistMatrixPerIndepVar
readSUDistMulti n path
    | takeExtension path == ".cbor" = do
        hPutStrLn stderr "Reading symmetric multidimensional distances"
        hPutStrLn stderr $ "Deserialising " ++ path
        res <- S.readFileDeserialise path
        hPutStrLn stderr "Done"
        return res
    | otherwise = do
        hPutStrLn stderr "Reading symmetric multidimensional distances"
        let nHalf = n*(n+1) `div` 2 -- length of packed upper triangle
        -- read entire file into memory
        !raw <- Bchs.readFile path
        let ls = Bchs.lines raw
        when (null ls) $ throwL "empty file"
        -- parse header: id1, id2, then indep vars
        let headerBS    = head ls
            allNames    = V.fromList (map Bchs.unpack (Bchs.split '\t' headerBS))
            namesV      = V.filter (\nm -> nm /= "id1" && nm /= "id2") allNames
            stride      = V.length namesV
            indepIndices = V.findIndices (\nm -> nm /= "id1" && nm /= "id2") allNames
        -- allocate packed half-matrix vectors for each indep var
        matsMV <- V.forM (V.enumFromN (0 :: Int) stride) $ const (VSM.new nHalf)
        -- data rows (assumed in packed upper triangle order)
        let dataLines = tail ls
        -- would be a neat test, but requires reading list into memory:
        --     !lenRows = length dataLines
        -- when (lenRows /= nHalf) $
        --     throwL $ "row count mismatch: expected " ++ show nHalf ++ " got " ++ show lenRows
        -- process each data row
        let loop _ [] = pure ()
            loop !rowIx (bs:rest) = do
                let cols = Bchs.split '\t' bs
                -- write each indep var value to packed vector at rowIx
                forM_ [0..stride-1] $ \dimIx -> do
                    let colIx = indepIndices V.! dimIx
                        bsVal = cols !! colIx
                        !val  = case LexFrac.readDecimal bsVal of
                                  Just (d, _) -> d
                                  Nothing     -> throwL $ "invalid double " ++ Bchs.unpack (cols !! colIx) ++ " at row" ++ show rowIx
                    VSM.unsafeWrite (matsMV V.! dimIx) rowIx val
                when (rowIx `mod` 1000000 == 0) $ hPutStrLn stderr $ show rowIx
                loop (rowIx+1) rest
        loop 0 dataLines
        -- freeze
        frozen <- V.forM (V.zip namesV matsMV) $ \(name, mv) -> do
                     v <- VS.unsafeFreeze mv
                     pure (name, SUDistMatrix v)
        hPutStrLn stderr "Done"
        pure $ SUDistMatrixPerIndepVar (V.toList frozen)

readAUDistMulti :: Int -> Int -> FilePath -> IO AUDistMatrixPerIndepVar
readAUDistMulti nObs nGrid path
    | takeExtension path == ".cbor" = do
        hPutStrLn stderr "Reading asymmetric multidimensional distances"
        hPutStrLn stderr $ "Deserialising " ++ path
        res <- S.readFileDeserialise path
        hPutStrLn stderr "Done"
        return res
    | otherwise = do
        hPutStrLn stderr "Reading asymmetric multidimensional distances"
        let nTotal = nObs * nGrid
        -- read entire file into memory
        !raw <- Bchs.readFile path
        let ls = Bchs.lines raw
        when (null ls) $ error "Empty TSV file"
        -- parse header (tab-separated dimension names)
        let headerBS    = head ls
            allNames    = V.fromList (map Bchs.unpack (Bchs.split '\t' headerBS))
            namesV      = V.filter (\nm -> nm /= "obsID" && nm /= "gridID") allNames
            stride      = V.length namesV
            indepIndices = V.findIndices (\nm -> nm /= "obsID" && nm /= "gridID") allNames
        -- allocate one mutable vector per dimension
        matsMV <- V.forM (V.enumFromN (0 :: Int) stride) $ const (VSM.new nTotal)
        -- data rows
        let dataLines = tail ls
        -- would be a neat test, but requires reading list into memory:
        --     !nRows = length dataLines
        -- when (nRows /= nTotal) $
        --     throwL $ "row count mismatch: expected " ++ show nTotal ++ " got " ++ show nRows
        -- process each data row
        let loop _ [] = pure ()
            loop !rowIx (bs:rest) = do
                let cols = Bchs.split '\t' bs
                -- write each indep var value to packed vector at rowIx
                forM_ [0..stride-1] $ \dimIx -> do
                    let colIx = indepIndices V.! dimIx
                        !val  =  case LexFrac.readDecimal (cols !! colIx) of
                             Just (d, _) -> d
                             Nothing     -> error $ "Invalid double " ++ Bchs.unpack (cols !! colIx) ++ " at row" ++ show rowIx
                    VSM.unsafeWrite (matsMV V.! dimIx) rowIx val
                when (rowIx `mod` 1000000 == 0) $ hPutStrLn stderr $ show rowIx
                loop (rowIx+1) rest
        loop 0 dataLines
        -- freeze
        frozen <- V.forM (V.zip namesV matsMV) $ \(name, mv) -> do
                     v <- VS.unsafeFreeze mv
                     pure (name, AUDistMatrix nObs nGrid v)
        hPutStrLn stderr "Done"
        pure $ AUDistMatrixPerIndepVar (V.toList frozen)

readAUDist :: V.Vector Observation -> V.Vector IndepVarsDist -> FilePath -> IO AUDistMatrix
readAUDist obs grid path = do
    hPutStrLn stderr $ "Reading distances in " ++ path
    let nObs = V.length obs
        nGrid = V.length grid
    distVec <- Con.runConduitRes $
        sourceCSV path .|
        ConC.mapM unwrapCSVParsingErrors .|
        ConC.map (\(SpatDistObsGrid _ _ dist) -> dist) .|
        ConC.sinkVectorN (nObs * nGrid)
    hPutStrLn stderr "Done"
    return $ AUDistMatrix nGrid nObs distVec

readTempSamp :: V.Vector Observation -> FilePath -> IO TempSampleMatrix
readTempSamp obs path
    | takeExtension path == ".cbor" = do
        hPutStrLn stderr "Reading age samples"
        hPutStrLn stderr $ "Deserialising " ++ path
        hPutStrLn stderr "Warning: There is no input validation for serialised input"
        res <- S.readFileDeserialise path
        hPutStrLn stderr "Done"
        return res
    | otherwise = do
        hPutStrLn stderr "Reading age samples"
        hPutStrLn stderr $ "Parsing " ++ path
        let nObs = V.length obs
        -- determine number of samples to expect
        hPutStrLn stderr "Counting the number of age samples"
        nSamples <- Con.runConduitRes $
            sourceCSV path .|
            ConC.mapM unwrapCSVParsingErrors .|
            ConC.takeWhile (\(TempSample obsID _) -> obsID == _obsID (V.head obs)) .|
            ConC.length
        if nSamples > 0
        then hPutStrLn stderr $ "Expected age samples per observation: " ++ show nSamples
        else throwLIO $
                "Order of entries in --tempSampFile not equal to -i. " ++
                "Expected first value: " ++ _obsID (V.head obs)
        -- start the actual parsing
        sampleVec <- Con.runConduitRes $
               sourceCSV path
            .| ConC.mapM unwrapCSVParsingErrors
            .| checkOrder nSamples
            .| ConC.sinkVectorN (nObs * nSamples)
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
                                    liftIO $ throwLIO $
                                        "Order of entries in --tempSampFile not equal to -i. " ++
                                        "Expected: " ++ expected ++ " but got: " ++ obsID
                            Nothing -> return ()
                    loop [] = return ()

-- simpler parsers

readDepVarsPredGrid :: [String] -> [String] -> DepVarsPredGridSettings -> IO (V.Vector DepVarsPredPos)
readDepVarsPredGrid depVars _ (DirectDepVarsGridSettings depVarsPos) = do
    let depVarsPosReordered = V.map (filterByKey depVars) $ V.fromList depVarsPos
    return $ V.map DepVarsPredPosDirect depVarsPosReordered
readDepVarsPredGrid depVars indepVars (SearchObsDepVarsGridSettings path) = do
    !obs <- readObservations path -- search observations
    let obsFiltered = filterVarsInObs depVars indepVars obs
    return $ V.map DepVarsPredPosSearchObs obsFiltered

readObservations :: FilePath -> IO (V.Vector Observation)
readObservations path = do
    hPutStrLn stderr "Reading observations"
    res <- readToVector path
    let resWithID = V.zipWith setIndex res (V.generate (V.length res) id)
    return resWithID

readArbitraryDimPos :: FilePath -> IO (V.Vector ArbitraryDimPos)
readArbitraryDimPos path = do
    hPutStrLn stderr "Reading arbitrary-dimension grid positions"
    res <- readToVector path
    return res

readSpatPos :: FilePath -> IO (V.Vector SpatPos)
readSpatPos path = do
    hPutStrLn stderr "Reading spatial grid positions"
    res <- readToVector path
    let resWithID = V.zipWith setIndex res (V.generate (V.length res) id)
    return resWithID

readIndepVarsPos :: FilePath -> IO (V.Vector IndepVarsPos)
readIndepVarsPos path = do
    hPutStrLn stderr "Reading grid positions"
    res <- readToVector path
    return res

readToVector :: (Csv.FromNamedRecord a, S.Serialise a) => FilePath -> IO (V.Vector a)
readToVector path
    | takeExtension path == ".cbor" = do
        hPutStrLn stderr $ "Deserialising " ++ path
        hPutStrLn stderr "Warning: There is no input validation for serialised input"
        res <- S.readFileDeserialise path
        hPutStrLn stderr "Done"
        return res
    | otherwise = do
        hPutStrLn stderr $ "Parsing " ++ path
        res <- Con.runConduitRes $ sourceCSV path .| ConC.mapM unwrapCSVParsingErrors .| ConC.sinkVector
        hPutStrLn stderr "Done"
        return res

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
                Left e1  -> liftIO $ throwLIO $ show e1
                Right e2 -> liftIO $ throwLIO $ show e2
        Right res -> return res

sourceCSV :: (MonadResource m, MonadError IOError m, Csv.FromNamedRecord a) =>
                FilePath
             -> ConduitT () (Either (Either ConCsv.CsvStreamHaltParseError ConCsv.CsvStreamRecordParseError) a) m ()
sourceCSV path =
       ConC.sourceFile path
    .| ConCsv.fromNamedCsvStreamErrorNoThrow decodingOptions
    .| progress 1000000 Nothing

appendNamedCSV :: (MonadResource m, Csv.ToRecord a, Csv.DefaultOrdered a) => Maybe FilePath -> ConduitT a Void m ()
appendNamedCSV Nothing =
    Con.bracketP (return stdout) (const $ return ()) $ \handle ->
           ConCsv.toCsv encodingOptions
        .| ConC.mapM_ (liftIO . Bchs.hPutStr handle)
appendNamedCSV (Just path) =
    Con.bracketP (openFile path AppendMode) hClose $ \handle ->
           ConCsv.toCsv encodingOptions
        .| ConC.mapM_ (liftIO . Bchs.hPutStr handle)

sinkNamedCSV :: (MonadResource m, Csv.ToRecord a, Csv.DefaultOrdered a) => Maybe FilePath -> ConduitT a Void m ()
sinkNamedCSV Nothing =
    Con.bracketP (return stdout) (const $ return ()) $ \handle ->
           writeHeaderCSV handle
        .| ConCsv.toCsv encodingOptions
        .| ConC.mapM_ (liftIO . Bchs.hPutStr handle)
sinkNamedCSV (Just path) =
    Con.bracketP (openFile path WriteMode) hClose $ \handle ->
           writeHeaderCSV handle
        .| ConCsv.toCsv encodingOptions
        .| ConC.mapM_ (liftIO . Bchs.hPutStr handle)

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

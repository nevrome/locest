{-# LANGUAGE FlexibleContexts #-}

module LocEst.Parsers where

import LocEst.Types

import qualified Data.Csv                             as Csv
import qualified Data.Csv.Conduit                     as ConCsv
import Data.Conduit                         ((.|), ConduitT, Void)
import qualified Data.Conduit                         as Con
import qualified Data.Conduit.Combinators             as Con
import           Data.Char                            (ord)
import qualified Data.Conduit.List as ConL
import Conduit (MonadIO, liftIO, MonadResource)
import Data.IORef (newIORef, readIORef, modifyIORef)
import System.IO (stderr, hPutStrLn)
import Control.Monad.Error.Class

-- helper functions
decodingOptions :: Csv.DecodeOptions
decodingOptions = Csv.defaultDecodeOptions {
    Csv.decDelimiter = fromIntegral (ord '\t')
}

encodingOptions :: Csv.EncodeOptions
encodingOptions = Csv.defaultEncodeOptions {
      Csv.encDelimiter = fromIntegral (ord '\t')
    }

readSpatTempObs :: FilePath -> IO [SpatTempObs]
readSpatTempObs path =
    Con.runConduitRes $
           sourceCSV path
        .| ConL.consume

readSpatPos :: FilePath -> IO [SpatPos]
readSpatPos path =
    Con.runConduitRes $
           sourceCSV path
        .| ConL.consume

sourceCSV :: (MonadResource m, MonadError IOError m, Csv.FromNamedRecord a) => FilePath -> ConduitT () a m ()
sourceCSV path =
       Con.sourceFile path
    .| ConCsv.fromNamedCsvLiftError (userError . show) decodingOptions

sinkCSV :: (MonadResource m, Csv.ToRecord a) => FilePath -> ConduitT a Void m ()
sinkCSV path =
       ConCsv.toCsv encodingOptions
    .| Con.sinkFile path

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
            |  c `rem` 1000 == 0 = hPutStrLn stderr $ "Grid points: " ++ padLeft 7 (show c)
            -- |  c == 100          = putStrLn $ "Probing successful. Continuing now..."
            | otherwise = return ()

padLeft :: Int -> String -> String
padLeft n s
    | length s >= n = reverse (take n (reverse s))
    | length s < n = replicate (n - length s) ' ' ++ s
    | otherwise    = s
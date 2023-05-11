module LocEst.Parsers where

import LocEst.Types

import qualified Data.Csv                             as Csv
import qualified Data.Csv.Conduit                     as ConCsv
import Data.Conduit                         ((.|), ConduitT)
import qualified Data.Conduit                         as Con
import qualified Data.Conduit.Combinators             as Con
import           Data.Char                            (ord)
import qualified Data.Conduit.List as ConL
import qualified Data.Conduit.Algorithms.Async as ConAA
import qualified Data.Vector as V
import Conduit (MonadIO, liftIO)
import Data.IORef (newIORef, readIORef, modifyIORef)

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
           Con.sourceFile path
        .| ConCsv.fromNamedCsvLiftError (userError . show) decodingOptions
        .| ConL.consume

pipeSpatTempPosConduit :: FilePath -> FilePath -> (SpatTempPos -> SpatTempProb) -> IO ()
pipeSpatTempPosConduit inPath outPath f =
    Con.runConduitRes $
           Con.sourceFile inPath
        .| ConCsv.fromNamedCsvLiftError (userError . show) decodingOptions
        -- .| ConL.map f
        .| ConAA.asyncMapC 5 f
        -- .| Con.conduitVector 100 .| ConAA.asyncMapC 5 (V.map f) .| ConL.concat
        .| progress
        .| ConCsv.toCsv encodingOptions
        .| Con.sinkFile outPath

progress :: (MonadIO m) => ConduitT i i m ()
progress = do
    counterRef <- liftIO $ newIORef (0 :: Int)
    ConL.mapM_ $ \val -> do
        n <- liftIO $ readIORef counterRef
        liftIO $ logProgress n
        liftIO $ modifyIORef counterRef (+1)
    where
        logProgress :: Int -> IO ()
        logProgress c
            |  c `rem` 1000 == 0 = putStrLn $ "Grid points: " ++ padLeft 7 (show c)
            -- |  c == 100          = putStrLn $ "Probing successful. Continuing now..."
            | otherwise = return ()

padLeft :: Int -> String -> String
padLeft n s
    | length s >= n = reverse (take n (reverse s))
    | length s < n = replicate (n - length s) ' ' ++ s
    | otherwise    = s
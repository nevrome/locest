module LocEst.CLI.Utils where

import           LocEst.Types

import           Conduit           (MonadIO, liftIO)
import           Data.Conduit      (ConduitT)
import qualified Data.Conduit.List as ConC
import           Data.IORef        (modifyIORef, newIORef, readIORef)
import           GHC.Conc          (getNumCapabilities)
import           System.IO         (hPutStrLn, stderr)

--

setNumberOfThreads :: NumberOfThreads -> IO Int
setNumberOfThreads x = do
    numThreads <- set x
    hPutStrLn stderr $ "Working with threads: " ++ show numThreads
    return numThreads
    where
        set :: NumberOfThreads -> IO Int
        set SingleThread        = pure 1
        set (MultipleThreads n) = pure n
        set DetectThreads       = do
            detectedThreads <- getNumCapabilities
            hPutStrLn stderr $ "Detected max number of threads: " ++ show detectedThreads
            return detectedThreads

progress :: (MonadIO m) => Int -> Maybe Int -> ConduitT i i m ()
progress reportNum goal = do
    let goalString = case goal of
            Nothing -> ""
            Just x  -> "/" ++ show x
    counterRef <- liftIO $ newIORef (1 :: Int)
    ConC.mapM $ \val -> do
        n <- liftIO $ readIORef counterRef
        liftIO $ logProgress n goalString
        liftIO $ modifyIORef counterRef (+1)
        return val
    where
        logProgress :: Int -> String -> IO ()
        logProgress c g
            | c == 1 || c `rem` reportNum == 0 = hPutStrLn stderr $ "Iterations done: " ++ padLeft 9 (show c) ++ g
            | otherwise = return ()

padLeft :: Int -> String -> String
padLeft n s
    | length s >= n = reverse (take n (reverse s))
    | length s < n = replicate (n - length s) ' ' ++ s
    | otherwise    = s

forM :: Monad m => [a] -> (a -> m b) -> m [b]
forM = flip mapM
for :: [a] -> (a -> b) -> [b]
for = flip map
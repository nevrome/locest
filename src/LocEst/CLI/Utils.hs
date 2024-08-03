module LocEst.CLI.Utils where

import           LocEst.Types

import           Conduit           (MonadIO, liftIO)
import           Data.Conduit      (ConduitT)
import qualified Data.Conduit.List as ConC
import           Data.IORef        (modifyIORef, newIORef, readIORef)
import           GHC.Conc          (getNumCapabilities)
import           System.IO         (hPutStrLn, stderr)

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
    counterRef <- liftIO $ newIORef (1 :: Int)
    ConC.mapM $ \val -> do
        n <- liftIO $ readIORef counterRef
        liftIO $ logProgress n
        liftIO $ modifyIORef counterRef (+1)
        return val
    where
        logProgress :: Int -> IO ()
        logProgress c
            | c `rem` reportNum == 0 = do
                let stringDone = padLeft 9 (show c)
                    stringGoal = case goal of
                        Nothing -> ""
                        Just g  -> do
                            let division = (fromIntegral c / fromIntegral g) :: Double
                                percent = (fromInteger (round (division * 1000) :: Integer) / 10.0) :: Double
                                stringPercent = padLeft 8 (show percent) ++ "%"
                            "/" ++ show g ++ stringPercent
                hPutStrLn stderr $ "> " ++ stringDone ++ stringGoal
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

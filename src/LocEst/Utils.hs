{-# LANGUAGE DeriveGeneric #-}

module LocEst.Utils where

import           Control.DeepSeq   (NFData)
import           Control.Exception (Exception, throw, throwIO)
import           GHC.Generics      (Generic)
import           Conduit           (MonadIO, liftIO)
import           Data.Conduit      (ConduitT)
import qualified Data.Conduit.List as ConC
import           Data.IORef        (modifyIORef, newIORef, readIORef)
import           GHC.Conc          (getNumCapabilities)
import           System.IO         (hPutStrLn, stderr)

-- | Different exceptions for locest
newtype LocEstException = LocEstException String
    deriving (Show, Generic, Eq)

instance Exception LocEstException
instance NFData LocEstException

renderLocEstException :: LocEstException -> String
renderLocEstException (LocEstException s) = "\nError:\n" ++ s

throwL :: String -> a
throwL s = throw $ LocEstException s
throwLIO :: String -> IO a
throwLIO s = throwIO $ LocEstException s

inf :: Fractional a => a
inf = 1/0

nan :: Fractional a => a
nan = 0/0

setNumberOfThreads :: IO Int
setNumberOfThreads = do
    detectedThreads <- getNumCapabilities
    hPutStrLn stderr $ "Working with threads: " ++ show detectedThreads
    return detectedThreads

progress :: (MonadIO m) => Int -> Maybe Int -> ConduitT i i m ()
progress reportNum goal = do
    liftIO $ hPutStrLn stderr "Streaming..."
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
                let stringDone = padLeft 10 (show c)
                    stringGoal = case goal of
                        Nothing -> ""
                        Just g  -> do
                            let division = (fromIntegral c / fromIntegral g) :: Double
                                percent = (fromInteger (round (division * 1000) :: Integer) / 10.0) :: Double
                                stringPercent = padLeft 10 (show percent) ++ "%"
                            "/" ++ show g ++ stringPercent
                hPutStrLn stderr $ stringDone ++ stringGoal
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

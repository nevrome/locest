{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveGeneric #-}

module LocEst.Utils where

import           Conduit             (MonadIO, liftIO)
import           Control.DeepSeq     (NFData)
import           Control.Exception   (Exception, throw, throwIO)
import           Control.Monad.ST    (runST)
import           Data.Conduit        (ConduitT)
import qualified Data.Conduit.List   as ConC
import           Data.IORef          (modifyIORef, newIORef, readIORef)
import           Data.List           (sort)
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as VM
import           GHC.Generics        (Generic)
import           System.IO           (hPutStrLn, stderr)
import qualified System.Random       as R

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
                let stringDone = "Progress: " ++ padLeft 10 (show c)
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

median :: [Double] -> Double
median xs =
  let ys = sort xs
      n  = length ys
  in ys !! (n `div` 2)

shuffle :: V.Vector a -> R.StdGen -> (V.Vector a, R.StdGen)
shuffle vec0 gen0 =
    let n = V.length vec0
    in runST $ do
       mv <- V.thaw vec0
       let go !i !gen
             | i <= 1 = do
                 v <- V.freeze mv
                 pure (v, gen)
             | otherwise = do
                 let (j, gen') = R.randomR (0, i-1) gen
                 VM.swap mv (i-1) j
                 go (i-1) gen'
       go n gen0

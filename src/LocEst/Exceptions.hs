{-# LANGUAGE DeriveGeneric #-}

module LocEst.Exceptions where

import           Control.DeepSeq   (NFData)
import           Control.Exception (Exception, throw, throwIO)
import           GHC.Generics      (Generic)

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

{-# LANGUAGE DeriveGeneric #-}

module LocEst.Exceptions where

import           Control.DeepSeq   (NFData)
import           Control.Exception (Exception)
import           GHC.Generics      (Generic)

-- | Different exceptions for locest
data LOCESTException =
        NormalException String
      | ConfigFileParsingException String
      | CoreException String
    deriving (Show, Generic, Eq)

instance NFData LOCESTException

renderLOCESTException :: LOCESTException -> String
renderLOCESTException (NormalException s) =
    "\nError: \n" ++ s
renderLOCESTException (ConfigFileParsingException s) =
    "\nError: \n" ++ s
renderLOCESTException (CoreException s) =
    "\nError: \n" ++ s

instance Exception LOCESTException

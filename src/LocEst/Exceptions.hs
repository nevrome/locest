{-# LANGUAGE DeriveGeneric #-}

module LocEst.Exceptions where

import           Control.DeepSeq   (NFData)
import           Control.Exception (Exception)
import           GHC.Generics      (Generic)

-- | Different exceptions for locest
data LOCESTException =
        NormalException String
      | ConfigFileParsingException String
    deriving (Show, Generic, Eq)

instance NFData LOCESTException

renderLOCESTException :: LOCESTException -> String
renderLOCESTException (NormalException s) =
    "Error: " ++ s
renderLOCESTException (ConfigFileParsingException s) =
    "Error: \n" ++ s

instance Exception LOCESTException

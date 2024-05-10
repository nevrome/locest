{-# LANGUAGE DeriveGeneric #-}

module LocEst.Utils where

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

-- General helper functions

rightToJust :: Either a b -> Maybe b
rightToJust (Right x) = Just x
rightToJust _         = Nothing

leftToJust :: Either a b -> Maybe a
leftToJust (Left x) = Just x
leftToJust _        = Nothing

{-# LANGUAGE DeriveGeneric #-}

module LocEst.Utils where

import           Control.Exception    (Exception)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- | Different exceptions for locest
data LOCESTException =
      NormalException String -- ^ An exception for everything
    deriving (Show, Generic)

instance NFData LOCESTException

renderLOCESTException :: LOCESTException -> String
renderLOCESTException (NormalException s) =
    "<!> Error: " ++ s

instance Exception LOCESTException

-- General helper functions

rightToJust :: Either a b -> Maybe b
rightToJust (Right x) = Just x
rightToJust _         = Nothing
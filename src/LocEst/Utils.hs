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

-- helper functions

forM :: Monad m => [a] -> (a -> m b) -> m [b]
forM = flip mapM
for :: [a] -> (a -> b) -> [b]
for = flip map
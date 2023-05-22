module LocEst.Utils where

import           Control.Exception    (Exception)

-- | Different exceptions for locest
data LOCESTException =
      NormalException String -- ^ An exception for everything
    deriving (Show)

renderLOCESTException :: LOCESTException -> String
renderLOCESTException (NormalException s) =
    "<!> Error: " ++ s

instance Exception LOCESTException
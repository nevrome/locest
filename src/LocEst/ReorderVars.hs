module LocEst.ReorderVars where

import           LocEst.Types

import qualified Data.Vector  as V

-- the filtering takes place, because it must be possible to control the variables of interest
-- in the kernel definition
-- TODO: Remember what's the purpose of the ordering - I forgot


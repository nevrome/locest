module LocEst.CoreAlgorithms where

import           LocEst.Distance
import           LocEst.Exceptions
import           LocEst.MathUtils
import           LocEst.Types

import           Data.Bifunctor                    (second)
import           Data.List                         (sortBy)
import           Data.Maybe                        (catMaybes, mapMaybe)
import qualified Data.Vector                       as V
import qualified Data.Vector.Storable              as VS
import qualified Data.Vector.Unboxed               as VU
import qualified Numeric.LinearAlgebra             as M
import           Statistics.Distribution           (logDensity, quantile)
import           Statistics.Distribution.StudentT  (StudentT)
import           Statistics.Distribution.Transform (LinearTransform)



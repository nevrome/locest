{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE StrictData             #-}
{-# LANGUAGE BangPatterns           #-}

module LocEst.TypesFlat where

import LocEst.Types
import           LocEst.Exceptions     (throwL)

import qualified Data.Vector as V
import qualified Data.Vector.Storable   as VS
import           GHC.Generics          (Generic)
import qualified Codec.Serialise       as S
import           Control.DeepSeq

data IndepVarsDistFlat = IndepVarsDistFlat {
     tags    :: VS.Vector Bool -- False means IndepSpatTempDist, True means IndepArbitraryDimDist
   , payload :: VS.Vector Double -- distances stored contiguously per row.
   , stride  :: Int --  number of doubles per row (max of 2 or arbitrary dim length)
   }

-- | A data type for a symmetric, unidirectional distance matrix
-- this matrix has (n*n)/2 - n entries and a triangular shape
newtype SUDistMatrix = SUDistMatrix {
    _sudmMatrix     :: VS.Vector Double
} deriving (Generic, Show, Eq)
-- If you need a  lookup function for this matrix you must consider that the
-- triangular matrix packs its values in a certain order. In the case of a
-- lower triangular matrix, where every element above the principal diagonal
-- is zero, we can count by rows to get the right index for each value:
-- The first row contains 0 elements (as "a distance to itself" is not present),
-- The second row contains 1 element,
-- The third row contains 2 elements,
-- and so forth.
-- See https://math.stackexchange.com/questions/646117/how-to-find-a-function-mapping-matrix-indices

instance NFData SUDistMatrix

-- | A data type for named lists of matrices
newtype MatrixPerIndepVar = MatrixPerIndepVar [(IndepVarName, SUDistMatrix)]
    deriving (Generic, Show, Eq)

instance NFData MatrixPerIndepVar
instance PseudoMap MatrixPerIndepVar SUDistMatrix where
    toList (MatrixPerIndepVar l) = l
    getKeys (MatrixPerIndepVar l) = map fst l
    getValues (MatrixPerIndepVar l) = map snd l
    lookupUnsafe (MatrixPerIndepVar l) k =
        case lookup k l of
            Just x  -> x
            Nothing -> throwL $ "Failed lookup. Missing key: " ++ k
    allSameVars xs = allEqual $ map getKeys xs
    filterByKey k (MatrixPerIndepVar l) = MatrixPerIndepVar (filterByKeyList k l)

-- | A data type for an asymmetric, unidirectional distance matrix
-- this matrix has m*n different entries and a rectangular shape
data AUDistMatrix = AUDistMatrix {
      _audmNrCols :: Int -- column number
    , _audmNrRows :: Int -- row number
    , _audmMatrix :: VS.Vector Double
} deriving (Generic)

instance S.Serialise AUDistMatrix

lookUpDistanceAU :: AUDistMatrix -> Int -> Int -> Double
lookUpDistanceAU (AUDistMatrix ncol _ vec) col row = vec VS.! (col + ncol * row)

type SpatDistMatrix = AUDistMatrix

-- | A data type for a matrix with age samples for observations
data TempSampleMatrix = TempSampleMatrix {
      _tSMNrSamples :: Int -- column number
    , _tSMNrObs     :: Int -- row number
    , _tSMMatrix    :: VS.Vector YearBCAD
} deriving (Generic)

instance S.Serialise TempSampleMatrix

lookUpTempSample :: TempSampleMatrix -> Int -> Int -> YearBCAD
lookUpTempSample (TempSampleMatrix ncol _ vec) col row = vec VS.! (col + ncol * row)
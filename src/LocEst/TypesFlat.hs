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
   , stride  :: Int -- number of doubles per row (max of 2 or arbitrary dim length)
   }

-- | A data type for a symmetric, unidirectional distance matrix
-- this matrix has (n*n)/2 - n entries and a triangular shape
newtype SUDistMatrix = SUDistMatrix (VS.Vector Double)
   deriving (Generic, Show, Eq)

instance NFData SUDistMatrix

-- | A data type for named lists of matrices
newtype SUDistMatrixPerIndepVar = SUDistMatrixPerIndepVar [(IndepVarName, SUDistMatrix)]
    deriving (Generic, Show, Eq)

instance NFData SUDistMatrixPerIndepVar
instance PseudoMap SUDistMatrixPerIndepVar SUDistMatrix where
    toList (SUDistMatrixPerIndepVar l) = l
    getKeys (SUDistMatrixPerIndepVar l) = map fst l
    getValues (SUDistMatrixPerIndepVar l) = map snd l
    lookupUnsafe (SUDistMatrixPerIndepVar l) k =
        case lookup k l of
            Just x  -> x
            Nothing -> throwL $ "Failed lookup. Missing key: " ++ k
    allSameVars xs = allEqual $ map getKeys xs
    filterByKey k (SUDistMatrixPerIndepVar l) = SUDistMatrixPerIndepVar (filterByKeyList k l)

-- | A data type for an asymmetric, unidirectional distance matrix
-- this matrix has m*n different entries and a rectangular shape
data AUDistMatrix = AUDistMatrix {
      _audmNrCols :: Int -- column number
    , _audmNrRows :: Int -- row number
    , _audmMatrix :: VS.Vector Double
} deriving (Generic, Show, Eq)

instance NFData AUDistMatrix
instance S.Serialise AUDistMatrix

lookUpDistanceAU :: AUDistMatrix -> Int -> Int -> Double
lookUpDistanceAU (AUDistMatrix ncol _ vec) col row = vec VS.! (col + ncol * row)

type SpatDistMatrix = AUDistMatrix

-- | A data type for named lists of matrices
newtype AUDistMatrixPerIndepVar = AUDistMatrixPerIndepVar [(IndepVarName, AUDistMatrix)]
    deriving (Generic, Show, Eq)

instance NFData AUDistMatrixPerIndepVar
instance PseudoMap AUDistMatrixPerIndepVar AUDistMatrix where
    toList (AUDistMatrixPerIndepVar l) = l
    getKeys (AUDistMatrixPerIndepVar l) = map fst l
    getValues (AUDistMatrixPerIndepVar l) = map snd l
    lookupUnsafe (AUDistMatrixPerIndepVar l) k =
        case lookup k l of
            Just x  -> x
            Nothing -> throwL $ "Failed lookup. Missing key: " ++ k
    allSameVars xs = allEqual $ map getKeys xs
    filterByKey k (AUDistMatrixPerIndepVar l) = AUDistMatrixPerIndepVar (filterByKeyList k l)



-- | A data type for a matrix with age samples for observations
data TempSampleMatrix = TempSampleMatrix {
      _tSMNrSamples :: Int -- column number
    , _tSMNrObs     :: Int -- row number
    , _tSMMatrix    :: VS.Vector YearBCAD
} deriving (Generic)

instance S.Serialise TempSampleMatrix

lookUpTempSample :: TempSampleMatrix -> Int -> Int -> YearBCAD
lookUpTempSample (TempSampleMatrix ncol _ vec) col row = vec VS.! (col + ncol * row)
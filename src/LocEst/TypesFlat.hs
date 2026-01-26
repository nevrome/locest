{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE StrictData             #-}

module LocEst.TypesFlat where

import           LocEst.Types
import           LocEst.Utils         (throwL)

import qualified Codec.Serialise      as S
import           Control.DeepSeq
import qualified Data.Vector.Storable as VS
import           GHC.Generics         (Generic)

-- operations on the flat storage types

sliceSelfDistPerIndep :: VS.Vector Int -> SelfDistMatrixPerIndepVar -> SelfDistMatrixPerIndepVar
sliceSelfDistPerIndep idxs (SelfDistMatrixPerIndepVar ms) =
  SelfDistMatrixPerIndepVar [ (name, sliceSelfDistMatrix idxs m) | (name,m) <- ms ]

sliceCrossDistPerIndep :: VS.Vector Int -> VS.Vector Int -> SelfDistMatrixPerIndepVar -> CrossDistMatrixPerIndepVar
sliceCrossDistPerIndep testIdx trainIdx (SelfDistMatrixPerIndepVar ms) =
  CrossDistMatrixPerIndepVar [ (name, sliceCrossDistMatrix testIdx trainIdx m) | (name,m) <- ms ]

sliceSelfDistMatrix
    :: VS.Vector Int
    -> SelfDistMatrix
    -> SelfDistMatrix
sliceSelfDistMatrix idxs (SelfDistMatrix full) =
  let n = VS.length idxs
      nHalf = n*(n+1) `div` 2
      v = VS.generate nHalf $ \k ->
            let (i,j) = unHalf k
                oi = idxs VS.! i
                oj = idxs VS.! j
            in full VS.! idxHalf oi oj
  in SelfDistMatrix v
  where
    unHalf :: Int -> (Int, Int)
    unHalf k = go 0 k
      where
        go !i !r
          | r <= i    = (i, r)
          | otherwise = go (i+1) (r - (i+1))

sliceCrossDistMatrix
  :: VS.Vector Int  -- test indices (rows)
  -> VS.Vector Int  -- train indices (cols)
  -> SelfDistMatrix   -- full obs–obs
  -> CrossDistMatrix
sliceCrossDistMatrix testIdx trainIdx (SelfDistMatrix full) =
  let nGrid = VS.length testIdx
      nObs  = VS.length trainIdx
      v = VS.generate (nGrid * nObs) $ \k ->
            let (g,o) = k `divMod` nObs
                oi = trainIdx VS.! o
                gi = testIdx  VS.! g
            in full VS.! idxHalf oi gi
  in CrossDistMatrix nObs nGrid v

{-# INLINE idxHalf #-}
idxHalf :: Int -> Int -> Int
idxHalf i j
  | i >= j    = i*(i+1) `div` 2 + j
  | otherwise = j*(j+1) `div` 2 + i

-- flat storage data types

-- | A data type for distances between indep positions
data IndepVarsDistFlat = IndepVarsDistFlat {
     _tags    :: VS.Vector Bool -- False means IndepSpatTempDist, True means IndepArbitraryDimDist
   , _payload :: VS.Vector Double -- distances stored contiguously per row.
   , _stride  :: Int -- number of doubles per row (max of 2 or arbitrary dim length)
   } deriving (Generic)

instance NFData IndepVarsDistFlat

-- | A data type for a symmetric pairwise distance matrix within one set;
-- this matrix has (n*n)/2 - n entries and a triangular shape
newtype SelfDistMatrix = SelfDistMatrix (VS.Vector Double)
   deriving (Generic, Show, Eq)

instance NFData SelfDistMatrix
instance S.Serialise SelfDistMatrix

-- | A data type for named lists of matrices
newtype SelfDistMatrixPerIndepVar = SelfDistMatrixPerIndepVar [(IndepVarName, SelfDistMatrix)]
    deriving (Generic, Show, Eq)

instance NFData SelfDistMatrixPerIndepVar
instance S.Serialise SelfDistMatrixPerIndepVar
instance PseudoMap SelfDistMatrixPerIndepVar SelfDistMatrix where
    toList (SelfDistMatrixPerIndepVar l) = l
    getKeys (SelfDistMatrixPerIndepVar l) = map fst l
    getValues (SelfDistMatrixPerIndepVar l) = map snd l
    lookupUnsafe (SelfDistMatrixPerIndepVar l) k =
        case lookup k l of
            Just x  -> x
            Nothing -> throwL $ "Failed lookup. Missing key: " ++ k
    allSameVars xs = allEqual $ map getKeys xs
    filterByKey k (SelfDistMatrixPerIndepVar l) = SelfDistMatrixPerIndepVar (filterByKeyList k l)

-- | A data type for a symmetric pairwise distance matrix between two sets;
-- this matrix has m*n different entries and a rectangular shape
data CrossDistMatrix = CrossDistMatrix {
      _cdmNrCols :: Int -- column number
    , _cdmNrRows :: Int -- row number
    , _cdmMatrix :: VS.Vector Double
} deriving (Generic, Show, Eq)

instance NFData CrossDistMatrix
instance S.Serialise CrossDistMatrix

lookUpDistanceCross :: CrossDistMatrix -> Int -> Int -> Double
lookUpDistanceCross (CrossDistMatrix ncol _ vec) col row = vec VS.! (col + ncol * row)

type SpatDistMatrix = CrossDistMatrix

-- | A data type for named lists of matrices
newtype CrossDistMatrixPerIndepVar = CrossDistMatrixPerIndepVar [(IndepVarName, CrossDistMatrix)]
    deriving (Generic, Show, Eq)

instance NFData CrossDistMatrixPerIndepVar
instance S.Serialise CrossDistMatrixPerIndepVar
instance PseudoMap CrossDistMatrixPerIndepVar CrossDistMatrix where
    toList (CrossDistMatrixPerIndepVar l) = l
    getKeys (CrossDistMatrixPerIndepVar l) = map fst l
    getValues (CrossDistMatrixPerIndepVar l) = map snd l
    lookupUnsafe (CrossDistMatrixPerIndepVar l) k =
        case lookup k l of
            Just x  -> x
            Nothing -> throwL $ "Failed lookup. Missing key: " ++ k
    allSameVars xs = allEqual $ map getKeys xs
    filterByKey k (CrossDistMatrixPerIndepVar l) = CrossDistMatrixPerIndepVar (filterByKeyList k l)

-- | A data type for a matrix with age samples for observations
data TempSampleMatrix = TempSampleMatrix {
      _tSMNrSamples :: Int -- column number
    , _tSMNrObs     :: Int -- row number
    , _tSMMatrix    :: VS.Vector YearBCAD
} deriving (Generic)

instance S.Serialise TempSampleMatrix

lookUpTempSample :: TempSampleMatrix -> Int -> Int -> YearBCAD
lookUpTempSample (TempSampleMatrix ncol _ vec) col row = vec VS.! (col + ncol * row)

nrTempSamples :: Maybe TempSampleMatrix -> Int
nrTempSamples Nothing                         = 1
nrTempSamples (Just (TempSampleMatrix n _ _)) = n

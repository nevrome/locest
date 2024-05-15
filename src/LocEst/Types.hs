{-# LANGUAGE ApplicativeDo          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE StrictData             #-}

module LocEst.Types where

import LocEst.MathUtils

import qualified Codec.Serialise       as S
import           Control.Applicative   (empty, (<|>))
import           Control.DeepSeq
import qualified Data.ByteString.Char8 as Bchs
import qualified Data.Csv              as Csv
import qualified Data.HashMap.Strict   as HM
import           Data.List             (sortBy)
import           Data.Maybe            (catMaybes)
import qualified Data.Vector           as V
import qualified Data.Vector.Unboxed   as VU
import           GHC.Generics          (Generic)

-- typeclasses

-- a typeclass for maps
class PseudoMap a b | a -> b where
    getKeys :: a -> [String]
    getValues :: a -> [b]

-- a typeclass for things with ids
class Identifiable a where
    getID :: a -> String
    getIndex :: a -> Int
    setIndex :: a -> Int -> a

-- helper functions

-- lookup one column name
filterLookup :: Csv.FromField a => Csv.NamedRecord -> Bchs.ByteString -> Csv.Parser a
filterLookup m name = maybe empty Csv.parseField $ HM.lookup name m

-- lookup multiple column names and keep the first match
filterLookupMulti :: Csv.FromField a => Csv.NamedRecord -> [Bchs.ByteString] -> Csv.Parser a
filterLookupMulti m names =
    maybe empty Csv.parseField $ lookupMulti names
    where
        lookupMulti :: [Bchs.ByteString] -> Maybe Bchs.ByteString
        lookupMulti keys =
            let vals = map (`HM.lookup` m) keys
            in case catMaybes vals of
                []    -> Nothing
                (x:_) -> Just x

filterLookupOptional :: Csv.FromField a => Csv.NamedRecord -> Bchs.ByteString -> Csv.Parser (Maybe a)
filterLookupOptional m name = maybe (pure Nothing) Csv.parseField $ HM.lookup name m

-- data types

-- | A datatype for crossvalidation output
data CrossvalOutput = CrossvalOutput {
      _crossoutKernelDefinition :: KernelDefinition
    , _crossoutProbSum          :: Double
} deriving (Show, Generic)

instance NFData CrossvalOutput
-- these instances are a quick hack - should actually be defined down to the algo types:
instance Csv.DefaultOrdered CrossvalOutput where
    headerOrder (CrossvalOutput algo _) =
        Csv.headerOrder algo <> Csv.header ["probability"]
instance Csv.ToRecord CrossvalOutput where
    toRecord (CrossvalOutput algo sumProb) =
        Csv.toRecord algo <> Csv.record [Csv.toField sumProb]

-- | A datatype for an empirical variogram
newtype EmpiricalVariogram = EmpiricalVariogram [(Double, Double)]
    deriving Show

data EmpiricalVariogramOneVarCombination = EmpiricalVariogramOneVarCombination IndepVarName DepVarName EmpiricalVariogram
    deriving Show

data EmpiricalVariogramSingleBin = EmpiricalVariogramSingleBin IndepVarName DepVarName Double Double
    deriving Show

instance Csv.DefaultOrdered EmpiricalVariogramSingleBin where
    headerOrder _ = Csv.header ["indepVar", "depVar", "bin", "semivariance"]
instance Csv.ToRecord EmpiricalVariogramSingleBin where
    toRecord (EmpiricalVariogramSingleBin i d iv dv) =
        Csv.record [Csv.toField i, Csv.toField d, Csv.toField iv, Csv.toField dv]

-- | A datatype for normalization of the output
data Normalization = NormBySpace | NoNorm
    deriving (Show)

-- | A datatype for a symmetric, unidirectional distance matrix
-- this matrix has (n*n)/2 - n entries and a triangular shape
newtype SUDistMatrix = SUDistMatrix {
    _sudmMatrix     :: VU.Vector Double
} deriving (Generic, Show)

-- | This lookup function must consider that the triangular matrix packs
-- its values in a certain order. In the case of a lower triangular matrix,
-- where every element above the principal diagonal is zero, we can count
-- by rows to get the right index for each value:
-- The first row contains 0 elements (as "a distance to itself" is not present),
-- The second row contains 1 element,
-- The third row contains 2 elements,
-- and so forth.
-- see https://math.stackexchange.com/questions/646117/how-to-find-a-function-mapping-matrix-indices
-- for the lookup algorithm
lookUpDistanceSU :: SUDistMatrix -> Int -> Int -> Double
lookUpDistanceSU (SUDistMatrix vec) col row
    | col == row = 0
    | col < row  = vec VU.! (nodesInTriangle (row - 1) + col)
    | col > row  = vec VU.! (nodesInTriangle (col - 1) + row)
    | otherwise  = error "Impossible state in lookUpDistanceSU"
    where
        nodesInTriangle n = n * (n+1) `div` 2

-- | A datatype for an asymmetric, unidirectional distance matrix
-- this matrix has m*n different entries and a rectangular shape
data AUDistMatrix = AUDistMatrix {
      _audmNrCols :: Int -- column number
    , _audmNrRows :: Int -- row number
    , _audmMatrix :: VU.Vector Double
} deriving (Generic)

instance S.Serialise AUDistMatrix

lookUpDistanceAU :: AUDistMatrix -> Int -> Int -> Double
lookUpDistanceAU (AUDistMatrix ncol _ vec) col row = vec VU.! (col + ncol * row)

type SpatDistMatrix = AUDistMatrix

data SpatDistObsGrid = SpatDistObsGrid {
      _spatDistObsGridObsID    :: String
    , _spatDistObsGridGridID   :: String
    , _spatDistObsGridDistance :: Double
} deriving (Show, Generic)

instance NFData SpatDistObsGrid
instance Csv.FromNamedRecord SpatDistObsGrid where
    parseNamedRecord m =
        SpatDistObsGrid <$> filterLookup m "obsID" <*> filterLookup m "spatID" <*> filterLookup m "dist"

-- | A datatype for possible output of the core algorithm
data CoreOut =
      CoreObsWeight (V.Vector ObsWeight)
    | CoreSearchResult SearchResult
    deriving (Generic)

instance NFData CoreOut

-- | A datatype for observation weights per core permutation
data ObsWeight = ObsWeight {
      _powCorePermutation :: CorePermutation
    , _powObservation     :: ObsWithWeights
    } deriving (Generic)

instance NFData ObsWeight
instance Csv.DefaultOrdered ObsWeight where
    headerOrder (ObsWeight corePermutation obsWithWeights) =
        Csv.headerOrder corePermutation <> Csv.headerOrder obsWithWeights
instance Csv.ToRecord ObsWeight where
    toRecord (ObsWeight corePermutation obsWithWeights) =
        Csv.toRecord corePermutation <> Csv.toRecord obsWithWeights

-- | A datatype for search result points in space and time
data SearchResult = 
      SearchResult {
        _srCorePermutation :: CorePermutation
      , _srInterpolation   :: InterpolationResult
      , _srProbability     :: Maybe Double
      } deriving (Show, Generic)

instance NFData SearchResult
instance Csv.DefaultOrdered SearchResult where
    headerOrder (SearchResult corePermutation interpolationResult Nothing) =
        Csv.headerOrder corePermutation <> Csv.headerOrder interpolationResult
    headerOrder (SearchResult corePermutation interpolationResult (Just _)) =
        Csv.headerOrder corePermutation <> Csv.headerOrder interpolationResult <> Csv.header ["probability"]
instance Csv.ToRecord SearchResult where
    toRecord (SearchResult corePermutation interpolationResult Nothing) =
        Csv.toRecord corePermutation <> Csv.toRecord interpolationResult
    toRecord (SearchResult corePermutation interpolationResult (Just prob)) =
        Csv.toRecord corePermutation <> Csv.toRecord interpolationResult <> Csv.record [Csv.toField prob]

data SpatTempProb = SpatTempProb {
      _stprCorePermutation :: CorePermutation
    , _stprprobability     :: Double
    -- to model the different densities per input point
    -- (which will certainly be necessary for debugging)
    -- SpatTempProb must somehow include also the source Observation
    -- Perhaps this could be implemented as a Maybe String for the Obs name?
} deriving (Show, Generic)

instance NFData SpatTempProb
instance Csv.DefaultOrdered SpatTempProb where
    headerOrder (SpatTempProb spatTempDepVarsPos _) =
        Csv.headerOrder spatTempDepVarsPos <> Csv.header ["probability"]
instance Csv.ToRecord SpatTempProb where
    toRecord (SpatTempProb spatTempDepVarsPos prob) =
        Csv.toRecord spatTempDepVarsPos <> Csv.record [Csv.toField prob]

data SearchGrid = SearchGrid {
      _searchPosIndepVarsGrid :: IndepVarsPredGrid
    , _searchPosDepVarsGrid   :: Maybe DepVarsPredGrid
}

data IndepVarsPredGrid =
    SpaceTimeGrid {
      _stGridSpatPos         :: V.Vector SpatPos
    , _stGridTempPos         :: [Int]
    , _stGridSpaceTimeFilter :: Maybe (Double, Double)
    , _stGridSpatDist        :: Maybe SpatDistMatrix
    , _stGridTempSamples     :: Maybe TempSampleMatrix
    } |
    ArbitraryDimGrid {
      _adGridPos  :: V.Vector ArbitraryDimPos
    }

data DepVarsPredGrid = DepVarsPredGrid {
      _depVarsGrid  :: [DepVarsPos]
}

data CoreSupplement = CoreSupplement {
      _csSpaceTimeFilter :: Maybe (Double, Double)
    , _csSpatDist        :: Maybe SpatDistMatrix
    , _csTempSamp        :: Maybe TempSampleMatrix
}

-- | A datatype with core-algorithm settings
data CorePermutation = CorePermutation {
      _casIndepVarsPos          :: IndepVarsPos
    , _casDepVarsPos            :: Maybe DepVarsPos
    , _casKernelDefinition      :: KernelDefinition
    , _casTempSamplingIteration :: Int
} deriving (Show, Generic)

instance NFData CorePermutation
instance Csv.DefaultOrdered CorePermutation where
    headerOrder (CorePermutation indepVarsPos (Just depVarsPos) algorithm _) =
           Csv.headerOrder indepVarsPos
        <> V.map ("search_" <>) (Csv.headerOrder depVarsPos)
        <> Csv.headerOrder algorithm
        <> Csv.header ["temp_sampling_iteration"]
    headerOrder (CorePermutation indepVarsPos Nothing algorithm _) =
           Csv.headerOrder indepVarsPos
        <> Csv.headerOrder algorithm
        <> Csv.header ["temp_sampling_iteration"]
instance Csv.ToRecord CorePermutation where
    toRecord (CorePermutation indepVarsPos (Just depVarsPos) algorithm tempSamplingIteration) =
           Csv.toRecord indepVarsPos
        <> Csv.toRecord depVarsPos
        <> Csv.toRecord algorithm
        <> Csv.record [Csv.toField tempSamplingIteration]
    toRecord (CorePermutation indepVarsPos Nothing algorithm tempSamplingIteration) =
           Csv.toRecord indepVarsPos
        <> Csv.toRecord algorithm
        <> Csv.record [Csv.toField tempSamplingIteration]

type DepVarName = String

newtype KernelDefinition = KernelDefinition [KernelOneDepVar]
    deriving (Show, Eq, Ord, Generic)

instance NFData KernelDefinition
-- the following instances differ and don't use the KernelOneDepVar instance definitions:
-- there is a conceptual difference between looking at the complete KernelDefinition, which typically exists
-- in one row, and the KernelOneDepVar values, which can form an own table for input and output
instance Csv.DefaultOrdered KernelDefinition where
    headerOrder (KernelDefinition l) =
        Csv.header $ map (\x -> Bchs.pack $ "kernel_" ++ x) $ concatMap oneColSet l
        where
            oneColSet :: KernelOneDepVar -> [String]
            oneColSet (KernelOneDepVar name _ _ lengths) =
                let lengthscaleCols = map (++ "_length") $ getKeys lengths
                in map (\x -> name ++ "_" ++ x) $ "shape":"nugget":lengthscaleCols
instance Csv.ToRecord KernelDefinition where
    toRecord (KernelDefinition l) =
        V.concatMap oneColSet $ V.fromList l
        where
            oneColSet :: KernelOneDepVar -> Csv.Record
            oneColSet (KernelOneDepVar _ shape nugget lengths) =
                Csv.record [Csv.toField shape] <> Csv.record [Csv.toField nugget] <> Csv.toRecord lengths
instance PseudoMap KernelDefinition KernelLengths where
    getKeys   (KernelDefinition l) = map _kodvDepVarName l
    getValues (KernelDefinition l) = map _kodvLengths l

data KernelOneDepVar = KernelOneDepVar {
      _kodvDepVarName :: DepVarName
    , _kodvShape      :: KernelShape
    , _kodvNugget     :: KernelNugget
    , _kodvLengths    :: KernelLengths
    }
    deriving (Show, Eq, Ord, Generic)

instance NFData KernelOneDepVar
instance Csv.FromNamedRecord KernelOneDepVar where
    parseNamedRecord m = do
        depVarName <- filterLookup m "depVar"
        shape      <- filterLookup m "shape"
        nugget     <- filterLookup m "nugget"
        lengths    <- Csv.parseNamedRecord m
        pure $ KernelOneDepVar {
              _kodvDepVarName = depVarName
            , _kodvShape      = shape
            , _kodvNugget     = nugget
            , _kodvLengths    = lengths
            }
instance Csv.DefaultOrdered KernelOneDepVar where
    headerOrder (KernelOneDepVar _ _ _ lengths) =
        Csv.header ["depVar"] <>  Csv.header ["shape"] <> Csv.header ["nugget"] <> Csv.headerOrder lengths
instance Csv.ToRecord KernelOneDepVar where
    toRecord (KernelOneDepVar name shape nugget lengths) =
        Csv.toRecord name <> Csv.toRecord [Csv.toField shape] <> Csv.record [Csv.toField nugget] <> Csv.toRecord lengths

type IndepVarName = String
type KernelNugget = Double

data KernelLengths = KernelLengths ArbitraryDimPos
    deriving (Show, Eq, Ord, Generic)

instance NFData KernelLengths
instance Csv.FromNamedRecord KernelLengths where
    parseNamedRecord = Csv.parseNamedRecord
instance Csv.DefaultOrdered KernelLengths where
    headerOrder (KernelLengths arbitraryDimPos) = Csv.headerOrder arbitraryDimPos
instance Csv.ToRecord KernelLengths where
    toRecord (KernelLengths arbitraryDimPos) = Csv.toRecord arbitraryDimPos
instance PseudoMap KernelLengths Double where
    getKeys   (KernelLengths arbitraryDimPos) = getKeys arbitraryDimPos
    getValues (KernelLengths arbitraryDimPos) = getValues arbitraryDimPos

-- Data types for core algorithm specification
data KernelShape =
      SquaredExponential
    | Linear
    deriving (Show, Eq, Ord, Generic)

instance NFData KernelShape
instance Csv.FromField KernelShape where
    parseField x = Csv.parseField x >>= makeKernelShape
instance Csv.ToField KernelShape where
    toField SquaredExponential = "SqEx"
    toField Linear             = "Linear"

makeKernelShape :: MonadFail m => String -> m KernelShape
makeKernelShape "SqEx"   = pure SquaredExponential
makeKernelShape "Linear" = pure Linear
makeKernelShape x        = fail $ "Kernel shape " ++ show x ++ " not recognized"

data ObsWithWeights = ObsWithWeights {
      _owdObservation      :: Observation
    , _owdSpatTempDist     :: IndepVarsDist
    , _owdPerDepVarWeights :: DepVarsPos
} deriving (Generic)

instance NFData ObsWithWeights
instance Csv.DefaultOrdered ObsWithWeights where
    headerOrder (ObsWithWeights obs dists depVarWeights) =
        Csv.headerOrder obs <> Csv.headerOrder depVarWeights
instance Csv.ToRecord ObsWithWeights where
    toRecord (ObsWithWeights obs dists depVarWeights) =
        Csv.toRecord obs <> Csv.toRecord depVarWeights

data IndepVarsDist = IndepSpatTempDist SpatTempDist | IndepArbitraryDimDist [Double]
    deriving (Generic)

instance NFData IndepVarsDist

-- | A datatype for observations with id and position
data Observation = Observation {
      _obsIndex :: Int
    , _obsID    :: String
    , _obsPos   :: HyperPos
} deriving (Show, Generic)

instance S.Serialise Observation
instance NFData Observation
instance Csv.FromNamedRecord Observation where
    parseNamedRecord m = do
        identifier <- filterLookup m "obsID"
        position <- Csv.parseNamedRecord m
        pure $ Observation {
              _obsIndex = 0
            , _obsID = identifier
            , _obsPos = position
            }
instance Csv.DefaultOrdered Observation where
    headerOrder (Observation _ _ position) =
        Csv.header ["obsID"] <> Csv.headerOrder position
instance Csv.ToRecord Observation where
    toRecord (Observation _ identifier position) =
        Csv.toRecord identifier <> Csv.toRecord position
instance Identifiable Observation where
    getID (Observation _ identifier _) = identifier
    getIndex (Observation index _ _) = index
    setIndex x i = x {_obsIndex = i}

-- | A datatype for positions in independent and dependent var space
data HyperPos = HyperPos {
      _hyposIndepVarsPos :: IndepVarsPos
    , _hyposDepVarsPos   :: DepVarsPos
} deriving (Show, Generic)

instance S.Serialise HyperPos
instance NFData HyperPos
instance Csv.FromNamedRecord HyperPos where
    parseNamedRecord m = do
        indepVarsPos <- Csv.parseNamedRecord m
        depVarsPos <- Csv.parseNamedRecord m
        pure $ HyperPos {
              _hyposIndepVarsPos = indepVarsPos
            , _hyposDepVarsPos   = depVarsPos
            }
instance Csv.DefaultOrdered HyperPos where
    headerOrder (HyperPos indepVarsPos depVarsPos) =
        Csv.headerOrder indepVarsPos <> Csv.headerOrder depVarsPos
instance Csv.ToRecord HyperPos where
    toRecord (HyperPos indepVarsPos depVarsPos) =
        Csv.toRecord indepVarsPos <> Csv.toRecord depVarsPos

-- | A datatype for the interpolation output
newtype InterpolationResult = InterpolationResult [InterpolationResultOneDepVar]
    deriving (Eq, Show, Generic)

instance NFData InterpolationResult
instance Csv.DefaultOrdered InterpolationResult where
    headerOrder (InterpolationResult l) = V.concat $ map Csv.headerOrder l
instance Csv.ToRecord InterpolationResult where
    toRecord (InterpolationResult l) = V.concat $ map Csv.toRecord l

getProbability :: InterpolationResultOneDepVar -> Maybe Double
getProbability (InterpolationResultOneDepVarShort {}) = error "should never happen"
getProbability i@(InterpolationResultOneDepVarFull {})  = _irodvProbability i

data InterpolationResultOneDepVar =
      InterpolationResultOneDepVarShort {
          _irodvsDepVarName  :: DepVarName   -- name of the dependent variable
        , _irodvsLowerBound  :: OutInfDouble -- lower boundary of the 95% interval
        , _irodvsMedian      :: Double       -- median
        , _irodvsUpperBound  :: OutInfDouble -- upper boundary of the 95% interval
    } 
    | InterpolationResultOneDepVarFull {
          _irodvDepVarName  :: DepVarName    -- name of the dependent variable
        , _irodvEffN        :: Double        -- effective number of samples
        , _irodvWeightedAvg :: Double        -- weighted average
        , _irodvWeightedVar :: Double        -- weighted variance
        , _irodvPosterior   :: OutBool       -- could a posterior distribution be calculated?
        , _irodvLowerBound  :: OutInfDouble  -- lower boundary of the 95% interval
        , _irodvMedian      :: Double        -- median
        , _irodvUpperBound  :: OutInfDouble  -- upper boundary of the 95% interval
        , _irodvProbability :: Maybe Double  -- Probability for search value
    } deriving (Eq, Show, Generic)

instance NFData InterpolationResultOneDepVar
instance Csv.DefaultOrdered InterpolationResultOneDepVar where
    headerOrder (InterpolationResultOneDepVarShort n _ _ _ ) =
        Csv.header $ map (\x -> Bchs.pack $ "interpol_" ++ n ++ "_" ++ x) ["low", "median", "up"]
    headerOrder (InterpolationResultOneDepVarFull n _ _ _ _ _ _ _ (Just _)) =
        Csv.header $ map (\x -> Bchs.pack $ "interpol_" ++ n ++ "_" ++ x) ["neff", "avg", "var", "post", "low", "median", "up", "prob"]
    headerOrder (InterpolationResultOneDepVarFull n _ _ _ _ _ _ _ Nothing) =
        Csv.header $ map (\x -> Bchs.pack $ "interpol_" ++ n ++ "_" ++ x) ["neff", "avg", "var", "post", "low", "median", "up"]
instance Csv.ToRecord InterpolationResultOneDepVar where
    toRecord (InterpolationResultOneDepVarShort _ lb m ub) =
        Csv.record [ Csv.toField lb, Csv.toField m, Csv.toField ub ]
    toRecord (InterpolationResultOneDepVarFull _ neff a v po lb m ub (Just p)) =
        Csv.record [
            Csv.toField neff, Csv.toField a, Csv.toField v, Csv.toField po, Csv.toField lb, Csv.toField m, Csv.toField ub, Csv.toField p
        ]
    toRecord (InterpolationResultOneDepVarFull _ neff a v po lb m ub Nothing) =
        Csv.record [
            Csv.toField neff, Csv.toField a, Csv.toField v, Csv.toField po, Csv.toField lb, Csv.toField m, Csv.toField ub
        ]

resOneDepvar2Short :: InterpolationResultOneDepVar -> InterpolationResultOneDepVar
resOneDepvar2Short (InterpolationResultOneDepVarFull n _ _ _ _ lb m ub _) =
    InterpolationResultOneDepVarShort n lb m ub
resOneDepvar2Short x = x

newtype OutBool = OutBool Bool
    deriving (Eq, Show, Generic)
instance NFData OutBool
instance Csv.ToField OutBool where
    toField (OutBool True)  = "TRUE"
    toField (OutBool False) = "FALSE"

newtype OutInfDouble = OutInfDouble Double
    deriving (Eq, Show, Generic)
instance NFData OutInfDouble
instance Csv.ToField OutInfDouble where
    toField (OutInfDouble x)
        | x == infinity    = "Inf"
        | x == (-infinity) = "-Inf"
        | otherwise        = Bchs.pack $ show x

-- | A datatype for dependent vars
newtype DepVarsPos = DepVarsPos [(DepVarName, Double)]
    deriving (Eq, Show, Generic)

instance S.Serialise DepVarsPos
instance NFData DepVarsPos
instance Csv.FromNamedRecord DepVarsPos where
    parseNamedRecord m = do
        let extractedVarsBS = HM.filterWithKey (\k _ -> Bchs.isPrefixOf "dep" k) m
            extractedVarsStringDouble = HM.mapKeys Bchs.unpack $ HM.map (read . Bchs.unpack) extractedVarsBS
            sortedList = sortBy (\(k1,_) (k2,_) -> compare k1 k2) $ HM.toList extractedVarsStringDouble
        pure $ DepVarsPos sortedList
instance Csv.DefaultOrdered DepVarsPos where
    headerOrder (DepVarsPos l) =
        V.map Bchs.pack $ V.fromList $ map fst l
instance Csv.ToRecord DepVarsPos where
    toRecord (DepVarsPos l) =
        V.map (Bchs.pack . show) $ V.fromList $ map snd l
instance PseudoMap DepVarsPos Double where
    getKeys (DepVarsPos l) = map fst l
    getValues (DepVarsPos l) = map snd l

newtype ArbitraryDimPos = ArbitraryDimPos [(IndepVarName, Double)]
    deriving (Eq, Show, Ord, Generic)

instance S.Serialise ArbitraryDimPos
instance NFData ArbitraryDimPos
instance Csv.FromNamedRecord ArbitraryDimPos where
    parseNamedRecord m = do
        let extractedVarsBS = HM.filterWithKey (\k _ -> Bchs.isPrefixOf "indep" k) m
            extractedVarsStringDouble = HM.mapKeys Bchs.unpack $ HM.map (read . Bchs.unpack) extractedVarsBS
            sortedList = sortBy (\(k1,_) (k2,_) -> compare k1 k2) $ HM.toList extractedVarsStringDouble
        pure $ ArbitraryDimPos sortedList
instance Csv.DefaultOrdered ArbitraryDimPos where
    headerOrder (ArbitraryDimPos l) =
        V.map Bchs.pack $ V.fromList $ map fst l
instance Csv.ToRecord ArbitraryDimPos where
    toRecord (ArbitraryDimPos l) =
        V.map (Bchs.pack . show) $ V.fromList $ map snd l
instance PseudoMap ArbitraryDimPos Double where
    getKeys (ArbitraryDimPos l) = map fst l
    getValues (ArbitraryDimPos l) = map snd l

-- A datatype for positions in a spatiotemporal or an arbitrary space
data IndepVarsPos = IndepSpatTempPos SpatTempPos | IndepArbitraryDimPos ArbitraryDimPos
    deriving (Eq, Show, Generic)

instance S.Serialise IndepVarsPos
instance NFData IndepVarsPos
instance Csv.FromNamedRecord IndepVarsPos where
    parseNamedRecord m = do
        (IndepSpatTempPos <$> Csv.parseNamedRecord m) <|> (IndepArbitraryDimPos <$> Csv.parseNamedRecord m)
instance Csv.DefaultOrdered IndepVarsPos where
    headerOrder (IndepSpatTempPos x)     = Csv.headerOrder x
    headerOrder (IndepArbitraryDimPos x) = Csv.headerOrder x
instance Csv.ToRecord IndepVarsPos where
    toRecord (IndepSpatTempPos x)     = Csv.toRecord x
    toRecord (IndepArbitraryDimPos x) = Csv.toRecord x

-- | A datatype for distances in space and time
data SpatTempDist = SpatTempDist {
      _spatDist :: Double
    , _tempDist :: Double
} deriving Generic

instance NFData SpatTempDist

-- | A datatype for spatio-temporal positions
data SpatTempPos = SpatTempPos {
      _spatialPos  :: SpatPos
    , _temporalPos :: TempPos
} deriving (Eq, Show, Generic)

instance S.Serialise SpatTempPos
instance NFData SpatTempPos
instance Csv.FromNamedRecord SpatTempPos where
    parseNamedRecord m = do
        spatPos <- Csv.parseNamedRecord m
        tempPos <- Csv.parseNamedRecord m
        pure $ SpatTempPos {
              _spatialPos = spatPos
            , _temporalPos = tempPos
            }
instance Csv.DefaultOrdered SpatTempPos where
    headerOrder (SpatTempPos spatPos tempPos) =
        Csv.headerOrder spatPos <> Csv.headerOrder tempPos
instance Csv.ToRecord SpatTempPos where
    toRecord (SpatTempPos spatPos tempPos) =
        Csv.toRecord spatPos <> Csv.record [Csv.toField tempPos]

-- | A datatype for a matrix with age samples for observations
data TempSampleMatrix = TempSampleMatrix {
      _tSMNrSamples :: Int -- column number
    , _tSMNrObs     :: Int -- row number
    , _tSMMatrix    :: VU.Vector YearBCAD
} deriving (Generic)

instance S.Serialise TempSampleMatrix

lookUpTempSample :: TempSampleMatrix -> Int -> Int -> YearBCAD
lookUpTempSample (TempSampleMatrix ncol _ vec) col row = vec VU.! (col + ncol * row)

-- | A datatype for age samples
data TempSample = TempSample {
      _tempSampObsID :: String
    , _tempSampAge   :: YearBCAD
} deriving (Show, Generic)

instance NFData TempSample
instance Csv.FromNamedRecord TempSample where
    parseNamedRecord m =
        TempSample <$> filterLookupMulti m ["obsID", "id"] <*> filterLookup m "yearBCAD"

-- | A datatype for temporal positions
newtype TempPos = TempPos YearBCAD
    deriving (Eq, Show, Generic)

instance S.Serialise TempPos
instance NFData TempPos
instance Csv.FromNamedRecord TempPos where
    parseNamedRecord m = TempPos <$> filterLookup m "yearBCAD"
instance Csv.DefaultOrdered TempPos where
    headerOrder (TempPos _) = Csv.header ["yearBCAD"]
instance Csv.ToField TempPos where
    toField (TempPos x) = Csv.toField x

type YearBP = Word
type YearBCAD = Int
type YearRange = Word

-- | A datatype for spatial positions
data SpatPos = SpatPosCartesian CartesianPos | SpatPosLongLat LongLatPos
    deriving (Eq, Show, Generic)

instance S.Serialise SpatPos
instance NFData SpatPos
instance Csv.FromNamedRecord SpatPos where
    parseNamedRecord m = do
        (SpatPosCartesian <$> Csv.parseNamedRecord m) <|> (SpatPosLongLat <$> Csv.parseNamedRecord m)
instance Csv.DefaultOrdered SpatPos where
    headerOrder (SpatPosCartesian x) = Csv.headerOrder x
    headerOrder (SpatPosLongLat x)   = Csv.headerOrder x
instance Csv.ToRecord SpatPos where
    toRecord (SpatPosCartesian x) = Csv.toRecord x
    toRecord (SpatPosLongLat x)   = Csv.toRecord x
instance Identifiable SpatPos where
    getID (SpatPosCartesian x) = getID x
    getID (SpatPosLongLat x)   = getID x
    getIndex (SpatPosCartesian x) = getIndex x
    getIndex (SpatPosLongLat x)   = getIndex x
    setIndex (SpatPosCartesian x) i = SpatPosCartesian (setIndex x i)
    setIndex (SpatPosLongLat x) i   = SpatPosLongLat (setIndex x i)

-- | A datatype for projected coordinates
data CartesianPos = CartesianPos Int (Maybe String) Double Double
    deriving (Eq, Show, Generic)

instance S.Serialise CartesianPos
instance NFData CartesianPos
instance Csv.FromNamedRecord CartesianPos where
    parseNamedRecord m =
        CartesianPos <$> pure 0 <*> filterLookupOptional m "spatID" <*> filterLookup m "x" <*> filterLookup m "y"
instance Csv.DefaultOrdered CartesianPos where
    headerOrder _ = Csv.header ["spatID", "x", "y"]
instance Csv.ToRecord CartesianPos where
    toRecord (CartesianPos _ s x y) = Csv.record [Csv.toField s, Csv.toField x, Csv.toField y]
instance Identifiable CartesianPos where
    getID (CartesianPos _ Nothing _ _)           = "unnamed"
    getID (CartesianPos _ (Just identifier) _ _) = identifier
    getIndex (CartesianPos index _ _ _) = index
    setIndex (CartesianPos _ n c1 c2) i = CartesianPos i n c1 c2

-- | A datatype for Long-Lat coordinates
data LongLatPos = LongLatPos Int (Maybe String) Longitude Latitude
    deriving (Eq, Show, Generic)

instance S.Serialise LongLatPos
instance NFData LongLatPos
instance Csv.FromNamedRecord LongLatPos where
    parseNamedRecord m =
        LongLatPos <$> pure 0 <*> filterLookupOptional m "spatID" <*> filterLookup m "longitude" <*> filterLookup m "latitude"
instance Csv.DefaultOrdered LongLatPos where
    headerOrder _ = Csv.header ["spatID", "longitude", "latitude"]
instance Csv.ToRecord LongLatPos where
    toRecord (LongLatPos _ s long lat) = Csv.record [Csv.toField s, Csv.toField long, Csv.toField lat]
instance Identifiable LongLatPos where
    getID (LongLatPos _ Nothing _ _)           = "unnamed"
    getID (LongLatPos _ (Just identifier) _ _) = identifier
    getIndex (LongLatPos index _ _ _) = index
    setIndex (LongLatPos _ n c1 c2) i = LongLatPos i n c1 c2

-- | A datatype for Longitudes
newtype Longitude = Longitude Double
    deriving (Eq, Show, Generic)

makeLongitude :: MonadFail m => Double -> m Longitude
makeLongitude x
    | x >= -180 && x <= 180 = pure (Longitude x)
    | otherwise             = fail $ "Longitude " ++ show x ++ " not between -180 and 180"

instance S.Serialise Longitude
instance NFData Longitude
instance Csv.ToField Longitude where
    toField (Longitude x) = Csv.toField x
instance Csv.FromField Longitude where
    parseField x = Csv.parseField x >>= makeLongitude

-- | A datatype for Latitudes
newtype Latitude = Latitude Double
    deriving (Eq, Show, Generic)

makeLatitude :: MonadFail m => Double -> m Latitude
makeLatitude x
    | x >= -90 && x <= 90 = pure (Latitude x)
    | otherwise           = fail $ "Latitude " ++ show x ++ " not between -90 and 90"

instance S.Serialise Latitude
instance NFData Latitude
instance Csv.ToField Latitude where
    toField (Latitude x) = Csv.toField x
instance Csv.FromField Latitude where
    parseField x = Csv.parseField x >>= makeLatitude

data NumberOfThreads =
      SingleThread
    | MultipleThreads Int
    | DetectThreads
    deriving Show

{-# LANGUAGE ApplicativeDo          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE StrictData             #-}

module LocEst.Types where

import           LocEst.Exceptions     (throwL)
import           LocEst.MathUtils

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

-- a typeclass for data types with map-like properties
class PseudoMap a b | a -> b where
    getKeys :: a -> [String]
    getValues :: a -> [b]
    lookupUnsafe :: a -> String -> b

-- a typeclass for data types with ids
class Identifiable a where
    getID :: a -> String
    getIndex :: a -> Int
    setIndex :: a -> Int -> a

-- helper functions for cassava

-- lookup one column by name
filterLookup :: Csv.FromField a => Csv.NamedRecord -> Bchs.ByteString -> Csv.Parser a
filterLookup m name = maybe empty Csv.parseField $ HM.lookup name m

-- lookup optional column by name
filterLookupOptional :: Csv.FromField a => Csv.NamedRecord -> Bchs.ByteString -> Csv.Parser (Maybe a)
filterLookupOptional m name = maybe (pure Nothing) Csv.parseField $ HM.lookup name m

-- lookup column by multiple different names and keep the first match
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

-- data types

-- special types for the cross subcommand

-- | A data type for crossvalidation output
data CrossvalOutput = CrossvalOutput {
      _crossoutKernelDefinition :: KernelDefinition
    , _crossoutDistSum          :: Double
    , _crossoutDistMeanSquared  :: Double
    , _crossoutProbSum          :: Double
} deriving (Show, Generic)

instance NFData CrossvalOutput
instance Csv.DefaultOrdered CrossvalOutput where
    headerOrder (CrossvalOutput algo _ _ _) =
        Csv.headerOrder algo <> Csv.header ["sum_dep_dist_euclidean"] <> Csv.header ["mean_squared_dep_dist_euclidean"] <> Csv.header ["sum_log_likelihood"]
instance Csv.ToRecord CrossvalOutput where
    toRecord (CrossvalOutput algo sumDist meanSquaredDist sumProb) =
        Csv.toRecord algo <> Csv.record [Csv.toField sumDist] <> Csv.record [Csv.toField meanSquaredDist] <> Csv.record [Csv.toField $ OutInfDouble sumProb]

-- special types for the vario subcommands

-- | A data type for an empirical variogram
newtype EmpiricalVariogram = EmpiricalVariogram [((Double,Double,Double), Double)]
    deriving Show

data EmpiricalVariogramOneVarCombination = EmpiricalVariogramOneVarCombination IndepVarName DepVarName EmpiricalVariogram
    deriving Show

data EmpiricalVariogramSingleBin = EmpiricalVariogramSingleBin {
    _evIndepVar :: IndepVarName,
    _evDepVar   :: DepVarName,
    _evBin      :: (Double,Double,Double),
    _evVariance :: Double
    }
    deriving Show

instance Csv.DefaultOrdered EmpiricalVariogramSingleBin where
    headerOrder _ = Csv.header ["indepVar", "depVar", "bin_min", "bin_mid", "bin_max", "variance"]
instance Csv.ToRecord EmpiricalVariogramSingleBin where
    toRecord (EmpiricalVariogramSingleBin i d (bmin, bmid, bmax) dv) =
        Csv.record [Csv.toField i, Csv.toField d, Csv.toField bmin, Csv.toField bmid, Csv.toField bmax, Csv.toField dv]

-- general types or types specifically relevant for the search subcommand

-- | A data type for normalization of search output
data Normalization = NormBySpace | NoNorm
    deriving (Show)

-- | A data type for a symmetric, unidirectional distance matrix
-- this matrix has (n*n)/2 - n entries and a triangular shape
newtype SUDistMatrix = SUDistMatrix {
    _sudmMatrix     :: VU.Vector Double
} deriving (Generic, Show)
-- If you need a  lookup function for this matrix you must consider that the
-- triangular matrix packs its values in a certain order. In the case of a
-- lower triangular matrix, where every element above the principal diagonal
-- is zero, we can count by rows to get the right index for each value:
-- The first row contains 0 elements (as "a distance to itself" is not present),
-- The second row contains 1 element,
-- The third row contains 2 elements,
-- and so forth.
-- See https://math.stackexchange.com/questions/646117/how-to-find-a-function-mapping-matrix-indices

-- | A data type for an asymmetric, unidirectional distance matrix
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

-- | A data type for an individual distance between one observation and one prediction grid point.
-- Exists for reading from CSV into a SpatDistMatrix
data SpatDistObsGrid = SpatDistObsGrid {
      _spatDistObsGridObsID    :: String
    , _spatDistObsGridGridID   :: String
    , _spatDistObsGridDistance :: Double
} deriving (Show, Generic)

instance NFData SpatDistObsGrid
instance Csv.FromNamedRecord SpatDistObsGrid where
    parseNamedRecord m =
        SpatDistObsGrid <$> filterLookup m "obsID" <*> filterLookup m "spatID" <*> filterLookup m "dist"

-- | A data type for selecting the number of threads locest should use
data NumberOfThreads =
      SingleThread
    | MultipleThreads Int
    | DetectThreads
    deriving Show

-- | A data type for requesting specific output of the core algorithm
data CoreOutMode =
      CoreOutObsWeight Int
    | CoreOutInterpolSamples Int (Maybe Int) (Maybe SamplingRange)
    | CoreOutShort
    | CoreOutFull

data SamplingRange =
      OneSigma
    | TwoSigma
    | FullDistribution

-- | A data type for observation weights per core permutation
data ObsWeight = ObsWeight {
      _powCorePermutation :: CorePermutation
    , _powObsWeights      :: ObsWithWeights
    } deriving (Generic)

instance NFData ObsWeight
instance Csv.DefaultOrdered ObsWeight where
    headerOrder (ObsWeight corePermutation obsWithWeights) =
        Csv.headerOrder corePermutation <> Csv.headerOrder obsWithWeights
instance Csv.ToRecord ObsWeight where
    toRecord (ObsWeight corePermutation obsWithWeights) =
        Csv.toRecord corePermutation <> Csv.toRecord obsWithWeights

-- | A datatype for interpolation samples produced by the core algorithm
data InterpolationSample =
      InterpolationSample {
        _isCorePermutation       :: CorePermutation
      , _isInterpolRandIteration :: Int
      , _isInterpolRandSamples   :: DepVarSamples
      } deriving (Show, Generic)

instance NFData InterpolationSample
instance Csv.DefaultOrdered InterpolationSample where
    headerOrder (InterpolationSample corePermutation _ depVarSamples) =
        Csv.headerOrder corePermutation <> Csv.header ["random_iteration"] <> Csv.headerOrder depVarSamples
instance Csv.ToRecord InterpolationSample where
    toRecord (InterpolationSample corePermutation randIteration depVarSamples) =
        Csv.toRecord corePermutation <>  Csv.record [Csv.toField randIteration] <> Csv.toRecord depVarSamples

-- | A data type for search results produced by the core algorithm
data SearchResult =
      SearchResult {
        _srCorePermutation :: CorePermutation
      , _srInterpolation   :: InterpolationResult
      , _srLikelihood      :: Maybe SearchLikelihood
      } deriving (Show, Generic)

instance NFData SearchResult
instance Csv.DefaultOrdered SearchResult where
    headerOrder (SearchResult corePermutation interpolationResult Nothing) =
        Csv.headerOrder corePermutation <> Csv.headerOrder interpolationResult
    headerOrder (SearchResult corePermutation interpolationResult (Just searchLikelihood)) =
        Csv.headerOrder corePermutation <> Csv.headerOrder interpolationResult <> Csv.headerOrder searchLikelihood
instance Csv.ToRecord SearchResult where
    toRecord (SearchResult corePermutation interpolationResult Nothing) =
        Csv.toRecord corePermutation <> Csv.toRecord interpolationResult
    toRecord (SearchResult corePermutation interpolationResult (Just searchLikelihood)) =
        Csv.toRecord corePermutation <> Csv.toRecord interpolationResult <> Csv.toRecord searchLikelihood

-- | A data type specifically for the likelihood output of the core search
data SearchLikelihood = SearchLikelihood {
      _slhEuclideanDep  :: Double -- Euclidean distance in dependent variable space between interpolation and search depvar position
    , _slhLogLikelihood :: Double -- Likelihood of the search value
    , _slhProbability   :: Maybe Double -- Normalized likelihood (= probability) of the search depvar position
} deriving (Show, Generic)

instance NFData SearchLikelihood
instance Csv.DefaultOrdered SearchLikelihood where
    headerOrder (SearchLikelihood _ _ Nothing) =
        Csv.header ["dep_dist_euclidean", "log_likelihood"]
    headerOrder (SearchLikelihood _ _ (Just _)) =
        Csv.header ["dep_dist_euclidean", "log_likelihood", "probability"]
instance Csv.ToRecord SearchLikelihood where
    toRecord (SearchLikelihood depDist logLikelihood Nothing) =
        Csv.record [Csv.toField depDist, Csv.toField logLikelihood]
    toRecord (SearchLikelihood depDist logLikelihood (Just prob)) =
        Csv.record [Csv.toField depDist, Csv.toField logLikelihood, Csv.toField prob]

-- | A data type for the independent variable space prediction grid
data IndepVarsPredGrid =
    SpaceTimeGrid {
      _stGridSpatPos            :: V.Vector SpatPos
    , _stGridTempPos            :: [AbsRelTempPos]
    , _stGridSpaceTimeMinFilter :: (Double, Double)
    , _stGridSpaceTimeMaxFilter :: (Double, Double)
    , _stGridSpatDist           :: Maybe SpatDistMatrix
    , _stGridTempSamples        :: Maybe TempSampleMatrix
    } |
    ArbitraryDimGrid {
      _adGridPos  :: V.Vector ArbitraryDimPos
    }

-- | A data type for supplementary information used in the core algorithm
data CoreSupplement = CoreSupplement {
      _csSpaceTimeMinFilter :: (Double, Double)
    , _csSpaceTimeMaxFilter :: (Double, Double)
    , _csSpatDist           :: Maybe SpatDistMatrix
    , _csTempSamp           :: Maybe TempSampleMatrix
}

-- | A data type with core-algorithm settings (for one run of the core algorithm)
data CorePermutation = CorePermutation {
      _casIndepVarsPos          :: IndepVarsPos
    , _casSearchObs             :: Maybe DepVarsPredPos
    , _casKernelDefinition      :: KernelDefinition
    , _casTempSamplingIteration :: Int
    , _casCrossIteration        :: Int
} deriving (Show, Generic)

instance NFData CorePermutation
instance Csv.DefaultOrdered CorePermutation where
    headerOrder (CorePermutation indepVarsPos (Just depVarsPredPos) algorithm _ _) =
           Csv.headerOrder indepVarsPos
        <> Csv.headerOrder depVarsPredPos
        <> Csv.headerOrder algorithm
        <> Csv.header ["temp_sampling_iteration"]
        <> Csv.header ["cross_iteration"]
    headerOrder (CorePermutation indepVarsPos Nothing algorithm _ _) =
           Csv.headerOrder indepVarsPos
        <> Csv.headerOrder algorithm
        <> Csv.header ["temp_sampling_iteration"]
        <> Csv.header ["cross_iteration"]
instance Csv.ToRecord CorePermutation where
    toRecord (CorePermutation indepVarsPos (Just depVarsPredPos) algorithm tempSamplingIteration crossIteration) =
           Csv.toRecord indepVarsPos
        <> Csv.toRecord depVarsPredPos
        <> Csv.toRecord algorithm
        <> Csv.record [Csv.toField tempSamplingIteration]
        <> Csv.record [Csv.toField crossIteration]
    toRecord (CorePermutation indepVarsPos Nothing algorithm tempSamplingIteration crossIteration) =
           Csv.toRecord indepVarsPos
        <> Csv.toRecord algorithm
        <> Csv.record [Csv.toField tempSamplingIteration]
        <> Csv.record [Csv.toField crossIteration]

-- | A data type for a dependent variable space prediction grid
newtype DepVarsPredGrid = DepVarsPredGrid [DepVarsPredPos]

-- | A data type for individual dependent variable positions
data DepVarsPredPos =
      DepVarsPredPosDirect DepVarsPos
    | DepVarsPredPosSearchObs Observation
    deriving (Show, Generic, Eq)

instance NFData DepVarsPredPos
instance Csv.DefaultOrdered DepVarsPredPos where
    headerOrder (DepVarsPredPosDirect depVarsPos) =
           V.map ("search_" <>) $ Csv.headerOrder depVarsPos
    headerOrder (DepVarsPredPosSearchObs searchObs) =
           V.map ("search_" <>) $ Csv.headerOrder searchObs
instance Csv.ToRecord DepVarsPredPos where
    toRecord (DepVarsPredPosDirect depVarsPos) =
           Csv.toRecord depVarsPos
    toRecord (DepVarsPredPosSearchObs searchObs) =
           Csv.toRecord searchObs

-- | A data type to specify a kernel across multiple depvars and indepvars
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
instance PseudoMap KernelDefinition KernelOneDepVar where
    getKeys   (KernelDefinition l) = map _kodvDepVarName l
    getValues (KernelDefinition l) = l
    lookupUnsafe kernDef@(KernelDefinition _) k =
        let kernList = zip (getKeys kernDef) (getValues kernDef)
        in case lookup k kernList of
            Just x  -> x
            Nothing -> throwL $ "Failed lookup. Kernel definition must be incomplete. Missing key: " ++ k

-- | A data type for a component of a kernel definition for one depvar
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

-- type definitions for easier readability
type DepVarName   = String
type IndepVarName = String
type KernelNugget = Double

-- | A data type for kernel lengthscale parameters for multiple indepvars
newtype KernelLengths = KernelLengths ArbitraryDimLengths
    deriving (Show, Eq, Ord, Generic)

instance NFData KernelLengths
instance Csv.FromNamedRecord KernelLengths where
    parseNamedRecord = Csv.parseNamedRecord
instance Csv.DefaultOrdered KernelLengths where
    headerOrder (KernelLengths arbitraryDimLengths) = Csv.headerOrder arbitraryDimLengths
instance Csv.ToRecord KernelLengths where
    toRecord (KernelLengths arbitraryDimLengths) = Csv.toRecord arbitraryDimLengths
instance PseudoMap KernelLengths Double where
    getKeys   (KernelLengths arbitraryDimLengths) = getKeys arbitraryDimLengths
    getValues (KernelLengths arbitraryDimLengths) = getValues arbitraryDimLengths
    lookupUnsafe (KernelLengths arbitraryDimLengths) k = lookupUnsafe arbitraryDimLengths k

-- | A data type for kernel shapes
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

-- | A data type for a observation with a distance and weight in relation to a point of interest
data ObsWithWeights = ObsWithWeights {
      _owdObservation      :: Observation
    , _owdSpatTempDist     :: IndepVarsDist
    , _owdPerDepVarWeights :: DepVarsWeights
} deriving (Generic)

instance NFData ObsWithWeights
instance Csv.DefaultOrdered ObsWithWeights where
    headerOrder (ObsWithWeights obs dists depVarWeights) =
        V.map ("in_obs_" <>) (Csv.headerOrder obs <> Csv.headerOrder dists <> V.map ("weight_" <>) (Csv.headerOrder depVarWeights))
instance Csv.ToRecord ObsWithWeights where
    toRecord (ObsWithWeights obs dists depVarWeights) =
        Csv.toRecord obs <> Csv.toRecord dists <> Csv.toRecord depVarWeights

-- | A data type for a per-dimension distances in independent variable space
data IndepVarsDist = IndepSpatTempDist SpatTempDist | IndepArbitraryDimDist ArbitraryDimDists
    deriving (Generic)

instance NFData IndepVarsDist
instance Csv.DefaultOrdered IndepVarsDist where
    headerOrder (IndepSpatTempDist spatTempDist) =
        Csv.headerOrder spatTempDist
    headerOrder (IndepArbitraryDimDist arbitraryDimDists) =
        V.map ("dist_" <>) $ Csv.headerOrder arbitraryDimDists
instance Csv.ToRecord IndepVarsDist where
    toRecord (IndepSpatTempDist spatTempDist) =
        Csv.toRecord spatTempDist
    toRecord (IndepArbitraryDimDist arbitraryDimDists) =
        Csv.toRecord arbitraryDimDists

-- | A data type for observations with id and position
data Observation = Observation {
      _obsIndex :: Int
    , _obsID    :: String
    , _obsPos   :: HyperPos
} deriving (Show, Generic, Eq)

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
        Csv.record [Csv.toField identifier] <> Csv.toRecord position
instance Identifiable Observation where
    getID (Observation _ identifier _) = identifier
    getIndex (Observation index _ _) = index
    setIndex x i = x {_obsIndex = i}

-- | A data type for positions in independent and dependent var space
data HyperPos = HyperPos {
      _hyposIndepVarsPos :: IndepVarsPos
    , _hyposDepVarsPos   :: DepVarsPos
} deriving (Show, Generic, Eq)

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

-- | A data type for the interpolation output
newtype InterpolationResult = InterpolationResult [InterpolationResultOneDepVar]
    deriving (Eq, Show, Generic)

instance NFData InterpolationResult
instance Csv.DefaultOrdered InterpolationResult where
    headerOrder (InterpolationResult l) = V.concat $ map Csv.headerOrder l
instance Csv.ToRecord InterpolationResult where
    toRecord (InterpolationResult l) = V.concat $ map Csv.toRecord l

getLogLikelihood :: InterpolationResultOneDepVar -> Maybe Double
getLogLikelihood (InterpolationResultOneDepVarShort {}) = error "should never happen"
getLogLikelihood i@(InterpolationResultOneDepVarFull {})  = _irodvLogLikelihood i

-- | A data type for interpolation output for one dependent variable
data InterpolationResultOneDepVar =
      InterpolationResultOneDepVarShort {
          _irodvsDepVarName :: DepVarName   -- name of the dependent variable
        , _irodvsLowerBound :: OutInfDouble -- lower boundary of the 95% interval
        , _irodvsMedian     :: Double       -- median
        , _irodvsUpperBound :: OutInfDouble -- upper boundary of the 95% interval
    }
    | InterpolationResultOneDepVarFull {
          _irodvDepVarName    :: DepVarName    -- name of the dependent variable
        , _irodvEffN          :: Double        -- effective number of samples
        , _irodvWeightedAvg   :: Double        -- weighted average
        , _irodvWeightedVar   :: Double        -- weighted variance
        , _irodvPosterior     :: OutBool       -- could a posterior distribution be calculated?
        , _irodvLowerBound    :: OutInfDouble  -- lower boundary of the 95% interval
        , _irodvMedian        :: Double        -- median
        , _irodvUpperBound    :: OutInfDouble  -- upper boundary of the 95% interval
        , _irodvLogLikelihood :: Maybe Double  -- Log-likelihood for search value
    } deriving (Eq, Show, Generic)

instance NFData InterpolationResultOneDepVar
instance Csv.DefaultOrdered InterpolationResultOneDepVar where
    headerOrder (InterpolationResultOneDepVarShort n _ _ _ ) =
        Csv.header $ map (\x -> Bchs.pack $ "interpol_" ++ n ++ "_" ++ x) ["low", "median", "up"]
    headerOrder (InterpolationResultOneDepVarFull n _ _ _ _ _ _ _ Nothing) =
        Csv.header $ map (\x -> Bchs.pack $ "interpol_" ++ n ++ "_" ++ x) ["neff", "avg", "var", "post", "low", "median", "up"]
    headerOrder (InterpolationResultOneDepVarFull n _ _ _ _ _ _ _ (Just _)) =
        Csv.header $ map (\x -> Bchs.pack $ "interpol_" ++ n ++ "_" ++ x) ["neff", "avg", "var", "post", "low", "median", "up", "logl"]
instance Csv.ToRecord InterpolationResultOneDepVar where
    toRecord (InterpolationResultOneDepVarShort _ lb m ub) =
        Csv.record [ Csv.toField lb, Csv.toField m, Csv.toField ub ]
    toRecord (InterpolationResultOneDepVarFull _ neff a v po lb m ub Nothing) =
        Csv.record [
            Csv.toField neff, Csv.toField a, Csv.toField v, Csv.toField po, Csv.toField lb, Csv.toField m, Csv.toField ub
        ]
    toRecord (InterpolationResultOneDepVarFull _ neff a v po lb m ub (Just l)) =
        Csv.record [
            Csv.toField neff, Csv.toField a, Csv.toField v, Csv.toField po, Csv.toField lb, Csv.toField m, Csv.toField ub, Csv.toField l
        ]

resOneDepvar2Short :: InterpolationResultOneDepVar -> InterpolationResultOneDepVar
resOneDepvar2Short (InterpolationResultOneDepVarFull n _ _ _ _ lb m ub _) =
    InterpolationResultOneDepVarShort n lb m ub
resOneDepvar2Short x = x

-- | A data type that wraps around bools to modify the way they are rendered in the .tsv output
-- This is specifically done to make it easily readable in R
newtype OutBool = OutBool Bool
    deriving (Eq, Show, Generic)
instance NFData OutBool
instance Csv.ToField OutBool where
    toField (OutBool True)  = "TRUE"
    toField (OutBool False) = "FALSE"

-- | A data type that wraps around Doubles to modify the way they are rendered in the .tsv output.
-- This is specifically done for the representation of infinity to make it easily readable in R
newtype OutInfDouble = OutInfDouble Double
    deriving (Eq, Generic)
instance NFData OutInfDouble
instance Csv.ToField OutInfDouble where
    toField (OutInfDouble x)
        | x == infinity    = "Inf"
        | x == (-infinity) = "-Inf"
        | otherwise        = Bchs.pack $ show x
instance Show OutInfDouble where
    show (OutInfDouble x)
        | x == infinity    = "Inf"
        | x == (-infinity) = "-Inf"
        | otherwise        = show x

-- | A data type for dependent vars with some value
type DepVarsPos = ValuesPerDepVar
type DepVarsWeights = ValuesPerDepVar
type DepVarsRands = ValuesPerDepVar
type DepVarSamples = ValuesPerDepVar
type DepVarVariances = ValuesPerDepVar
newtype ValuesPerDepVar = ValuesPerDepVar [(DepVarName, Double)]
    deriving (Eq, Show, Generic)

instance S.Serialise ValuesPerDepVar
instance NFData ValuesPerDepVar
instance Csv.FromNamedRecord ValuesPerDepVar where
    parseNamedRecord m = do
        let extractedVarsBS = HM.filterWithKey (\k _ -> Bchs.isPrefixOf "dep" k) m
            extractedVarsStringDouble = HM.mapKeys Bchs.unpack $ HM.map (read . Bchs.unpack) extractedVarsBS
            sortedList = sortBy (\(k1,_) (k2,_) -> compare k1 k2) $ HM.toList extractedVarsStringDouble
        pure $ ValuesPerDepVar sortedList
instance Csv.DefaultOrdered ValuesPerDepVar where
    headerOrder (ValuesPerDepVar l) =
        V.map Bchs.pack $ V.fromList $ map fst l
instance Csv.ToRecord ValuesPerDepVar where
    toRecord (ValuesPerDepVar l) =
        V.map (Bchs.pack . show) $ V.map OutInfDouble $ V.fromList $ map snd l
instance PseudoMap ValuesPerDepVar Double where
    getKeys (ValuesPerDepVar l) = map fst l
    getValues (ValuesPerDepVar l) = map snd l
    lookupUnsafe (ValuesPerDepVar l) k =
        case lookup k l of
            Just x  -> x
            Nothing -> throwL $ "Failed lookup. Some input must be incomplete. Missing key: " ++ k

-- | A data type for independent vars with some value
type ArbitraryDimPos = ValuesPerIndepVar
type ArbitraryDimDists = ValuesPerIndepVar
type ArbitraryDimLengths = ValuesPerIndepVar
newtype ValuesPerIndepVar = ValuesPerIndepVar [(IndepVarName, Double)]
    deriving (Eq, Show, Ord, Generic)

instance S.Serialise ValuesPerIndepVar
instance NFData ValuesPerIndepVar
instance Csv.FromNamedRecord ValuesPerIndepVar where
    parseNamedRecord m = do
        let extractedVarsBS = HM.filterWithKey (\k _ -> Bchs.isPrefixOf "indep" k) m
            extractedVarsStringDouble = HM.mapKeys Bchs.unpack $ HM.map (read . Bchs.unpack) extractedVarsBS
            sortedList = sortBy (\(k1,_) (k2,_) -> compare k1 k2) $ HM.toList extractedVarsStringDouble
        pure $ ValuesPerIndepVar sortedList
instance Csv.DefaultOrdered ValuesPerIndepVar where
    headerOrder (ValuesPerIndepVar l) =
        V.map Bchs.pack $ V.fromList $ map fst l
instance Csv.ToRecord ValuesPerIndepVar where
    toRecord (ValuesPerIndepVar l) =
        V.map (Bchs.pack . show) $ V.fromList $ map snd l
instance PseudoMap ValuesPerIndepVar Double where
    getKeys (ValuesPerIndepVar l) = map fst l
    getValues (ValuesPerIndepVar l) = map snd l
    lookupUnsafe (ValuesPerIndepVar l) k =
        case lookup k l of
            Just x  -> x
            Nothing -> throwL $ "Failed lookup. Some input must be incomplete. Missing key: " ++ k

-- A data type for positions independent variable space, so here either a spatiotemporal or an arbitrary space
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

-- | A data type for distances in space and time
data SpatTempDist = SpatTempDist {
      _spatDist :: Double
    , _tempDist :: Double
} deriving Generic

instance NFData SpatTempDist
instance Csv.DefaultOrdered SpatTempDist where
    headerOrder (SpatTempDist _ _) =
        Csv.header ["dist_space", "dist_time"]
instance Csv.ToRecord SpatTempDist where
    toRecord (SpatTempDist spatDist tempDist) =
        Csv.record [Csv.toField spatDist, Csv.toField tempDist]

-- | A data type for spatio-temporal positions
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

-- | A data type for a matrix with age samples for observations
data TempSampleMatrix = TempSampleMatrix {
      _tSMNrSamples :: Int -- column number
    , _tSMNrObs     :: Int -- row number
    , _tSMMatrix    :: VU.Vector YearBCAD
} deriving (Generic)

instance S.Serialise TempSampleMatrix

lookUpTempSample :: TempSampleMatrix -> Int -> Int -> YearBCAD
lookUpTempSample (TempSampleMatrix ncol _ vec) col row = vec VU.! (col + ncol * row)

-- | A data type for one age sample (used for reading from .tsv)
data TempSample = TempSample {
      _tempSampObsID :: String
    , _tempSampAge   :: YearBCAD
} deriving (Show, Generic)

instance NFData TempSample
instance Csv.FromNamedRecord TempSample where
    parseNamedRecord m =
        TempSample <$> filterLookupMulti m ["obsID", "id"] <*> filterLookup m "yearBCAD"

-- | A data type for temporal position input
data AbsRelTempPos = AbsTempPos YearBCAD | RelTempPos YearDist
    deriving (Eq, Show, Generic)

instance S.Serialise AbsRelTempPos
instance NFData AbsRelTempPos
instance Csv.FromNamedRecord AbsRelTempPos where
    parseNamedRecord m = (AbsTempPos <$> filterLookup m "yearBCAD") <|> (RelTempPos <$> filterLookup m "yearDist")
instance Csv.DefaultOrdered AbsRelTempPos where
    headerOrder (AbsTempPos _) = Csv.header ["yearBCAD"]
    headerOrder (RelTempPos _) = Csv.header ["yearDist"]
instance Csv.ToField AbsRelTempPos where
    toField (AbsTempPos x) = Csv.toField x
    toField (RelTempPos x) = Csv.toField x

-- | A data type for temporal positions
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
type YearDist = Int
type YearRange = Word

-- | A data type for spatial positions
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

-- | A data type for projected coordinates
data CartesianPos = CartesianPos Int (Maybe String) Double Double
    deriving (Eq, Show, Generic)

instance S.Serialise CartesianPos
instance NFData CartesianPos
instance Csv.FromNamedRecord CartesianPos where
    parseNamedRecord m =
        CartesianPos <$> pure 0 <*> filterLookupOptional m "spatID" <*> filterLookup m "x" <*> filterLookup m "y"
instance Csv.DefaultOrdered CartesianPos where
    headerOrder (CartesianPos _ Nothing _ _)  = Csv.header ["x", "y"]
    headerOrder (CartesianPos _ (Just _) _ _) = Csv.header ["spatID", "x", "y"]
instance Csv.ToRecord CartesianPos where
    toRecord (CartesianPos _ Nothing x y)  = Csv.record [Csv.toField x, Csv.toField y]
    toRecord (CartesianPos _ (Just s) x y) = Csv.record [Csv.toField s, Csv.toField x, Csv.toField y]
instance Identifiable CartesianPos where
    getID (CartesianPos _ Nothing _ _)           = "unnamed"
    getID (CartesianPos _ (Just identifier) _ _) = identifier
    getIndex (CartesianPos index _ _ _) = index
    setIndex (CartesianPos _ n c1 c2) i = CartesianPos i n c1 c2

-- | A data type for Long-Lat coordinates
data LongLatPos = LongLatPos Int (Maybe String) Longitude Latitude
    deriving (Eq, Show, Generic)

instance S.Serialise LongLatPos
instance NFData LongLatPos
instance Csv.FromNamedRecord LongLatPos where
    parseNamedRecord m =
        LongLatPos <$> pure 0 <*> filterLookupOptional m "spatID" <*> filterLookup m "longitude" <*> filterLookup m "latitude"
instance Csv.DefaultOrdered LongLatPos where
    headerOrder (LongLatPos _ Nothing _ _)  = Csv.header ["longitude", "latitude"]
    headerOrder (LongLatPos _ (Just _) _ _) = Csv.header ["spatID", "longitude", "latitude"]
instance Csv.ToRecord LongLatPos where
    toRecord (LongLatPos _ Nothing long lat)  = Csv.record [Csv.toField long, Csv.toField lat]
    toRecord (LongLatPos _ (Just s) long lat) = Csv.record [Csv.toField s, Csv.toField long, Csv.toField lat]
instance Identifiable LongLatPos where
    getID (LongLatPos _ Nothing _ _)           = "unnamed"
    getID (LongLatPos _ (Just identifier) _ _) = identifier
    getIndex (LongLatPos index _ _ _) = index
    setIndex (LongLatPos _ n c1 c2) i = LongLatPos i n c1 c2

-- | A data type for Longitudes
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

-- | A data type for Latitudes
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

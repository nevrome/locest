{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE FunctionalDependencies #-}

module LocEst.Types where

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

-- | A datatype for normalization of the output
data Normalization = NormBySpace | NoNorm
    deriving (Show)

-- | A datatype for an unidirectional distance matrix
data SpatDistMatrix = SpatDistMatrix {
      _sDMNrGridPoints :: Int -- column number
    , _sDMNrObs        :: Int -- row number
    , _sDMMatrix       :: VU.Vector Double
} deriving (Generic)

instance S.Serialise SpatDistMatrix

lookUpDistance :: SpatDistMatrix -> Int -> Int -> Double
lookUpDistance (SpatDistMatrix ncol _ vec) col row = vec VU.! (col + ncol * row)

data SpatDistObsGrid = SpatDistObsGrid {
      _spatDistObsGridObsID    :: String
    , _spatDistObsGridGridID   :: String
    , _spatDistObsGridDistance :: Double
} deriving (Show, Generic)

instance NFData SpatDistObsGrid
instance Csv.FromNamedRecord SpatDistObsGrid where
    parseNamedRecord m =
        SpatDistObsGrid <$> filterLookup m "obsID" <*> filterLookup m "spatID" <*> filterLookup m "dist"

-- | A datatype for search result points in space and time
data SearchResult = SearchResult {
      _srCorePermutation :: CorePermutation
    , _srInterpolation   :: InterpolationResult
    , _srProbability     :: Maybe Double
    -- to model the different densities per input point
    -- (which will certainly be necessary for debugging)
    -- SpatTempProb must somehow include also the source Observation
    -- Perhaps this could be implemented as a Maybe String for the Obs name?
} deriving (Show, Generic)

instance NFData SearchResult
instance Csv.DefaultOrdered SearchResult where
    headerOrder (SearchResult spatTempDepVarsPos interpolationResult Nothing) =
        Csv.headerOrder spatTempDepVarsPos <> Csv.headerOrder interpolationResult
    headerOrder (SearchResult spatTempDepVarsPos interpolationResult (Just _)) =
        Csv.headerOrder spatTempDepVarsPos <> Csv.headerOrder interpolationResult <> Csv.header ["probability"]
instance Csv.ToRecord SearchResult where
    toRecord (SearchResult spatTempDepVarsPos interpolationResult Nothing) =
        Csv.toRecord spatTempDepVarsPos <> Csv.toRecord interpolationResult
    toRecord (SearchResult spatTempDepVarsPos interpolationResult (Just prob)) =
        Csv.toRecord spatTempDepVarsPos <> Csv.toRecord interpolationResult <> Csv.record [Csv.toField prob]

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
      _stGridSpatPos         :: [SpatPos]
    , _stGridTempPos         :: [Int]
    , _stGridSpaceTimeFilter :: Maybe (Double, Double)
    , _stGridSpatDist        :: Maybe SpatDistMatrix
    , _stGridTempSamples     :: Maybe TempSampleMatrix
    } |
    ArbitraryDimGrid {
      _adGridPos  :: [ArbitraryDimPos]
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
    , _casAlgorithm             :: LocestAlgorithm
    , _casTempSamplingIteration :: Int
} deriving (Show, Generic)

instance NFData CorePermutation
instance Csv.DefaultOrdered CorePermutation where
    headerOrder (CorePermutation indepVarsPos (Just depVarsPos) algorithm _) =
           Csv.headerOrder indepVarsPos
        <> Csv.headerOrder depVarsPos
        <> Csv.headerOrder algorithm
        <> Csv.header ["tempSamplingIteration"]
    headerOrder (CorePermutation indepVarsPos Nothing algorithm _) =
           Csv.headerOrder indepVarsPos
        <> Csv.headerOrder algorithm
        <> Csv.header ["tempSamplingIteration"]
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

-- Data types for core algorithm specification
newtype LocestAlgorithm =
    AlgoKernSmooth {
        _aksKernelDefinition :: KernelDefinition
    }
    deriving (Show, Eq, Ord, Generic)

instance NFData LocestAlgorithm
instance Csv.DefaultOrdered LocestAlgorithm where
    headerOrder (AlgoKernSmooth kernDef) = Csv.headerOrder kernDef
instance Csv.ToRecord LocestAlgorithm where
    toRecord (AlgoKernSmooth kernDef) = Csv.toRecord kernDef

type DepVarName = String

newtype KernelDefinition = KernelDefinition [KernelOneDepVar]
    deriving (Show, Eq, Ord, Generic)

instance NFData KernelDefinition
instance Csv.DefaultOrdered KernelDefinition where
    headerOrder (KernelDefinition l) = Csv.headerOrder $ head l
instance Csv.ToRecord KernelDefinition where
    toRecord (KernelDefinition kernDef) =
        V.concatMap Csv.toRecord $ V.fromList kernDef
instance PseudoMap KernelDefinition Kernel where
    getKeys   (KernelDefinition l) = map _kodvDepVarName l
    getValues (KernelDefinition l) = map _kodvKernel l

data KernelOneDepVar = KernelOneDepVar {
      _kodvDepVarName :: DepVarName
    , _kodvNugget     :: Nugget
    , _kodvKernel     :: Kernel
    }
    deriving (Show, Eq, Ord, Generic)

instance NFData KernelOneDepVar
instance Csv.FromNamedRecord KernelOneDepVar where
    parseNamedRecord m = do
        depVarName <- filterLookup m "depVar"
        nugget     <- filterLookup m "nugget"
        kernel     <- Csv.parseNamedRecord m
        pure $ KernelOneDepVar {
              _kodvDepVarName = depVarName
            , _kodvNugget     = nugget
            , _kodvKernel     = kernel
            }
instance Csv.DefaultOrdered KernelOneDepVar where
    headerOrder (KernelOneDepVar _ _ kernel) =
        Csv.header ["depVar"] <> Csv.header ["nugget"] <> Csv.headerOrder kernel
instance Csv.ToRecord KernelOneDepVar where
    toRecord (KernelOneDepVar name nugget kernel) =
        Csv.toRecord name <> Csv.record [Csv.toField nugget] <> Csv.toRecord kernel

type IndepVarName = String
type Nugget = Double
type KernelWidth = Double

data Kernel =
        SquaredExponential ArbitraryDimPos
    deriving (Show, Eq, Ord, Generic)

instance NFData Kernel
instance Csv.FromNamedRecord Kernel where
    parseNamedRecord = Csv.parseNamedRecord
instance Csv.DefaultOrdered Kernel where
    headerOrder (SquaredExponential arbitraryDimPos) = Csv.headerOrder arbitraryDimPos
instance Csv.ToRecord Kernel where
    toRecord (SquaredExponential arbitraryDimPos) = Csv.toRecord arbitraryDimPos
instance PseudoMap Kernel Double where
    getKeys   (SquaredExponential arbitraryDimPos) = getKeys arbitraryDimPos
    getValues (SquaredExponential arbitraryDimPos) = getValues arbitraryDimPos

data ObsWithDist = ObsWithDist {
      _owdObservation  :: Observation
    , _owdSpatTempDist :: IndepVarsDist
}

data IndepVarsDist = IndepSpatTempDist SpatTempDist | IndepArbitraryDimDist [Double]

-- | A datatype for observations with id and position
data Observation = Observation {
      _obsIndex :: Int
    , _obsID    :: String
    , _obsPos   :: HyperPos
} deriving (Show, Generic)

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
    toRecord (InterpolationResult l) =
        V.concat $ map Csv.toRecord l

data InterpolationResultOneDepVar = InterpolationResultOneDepVar {
          _irodvDepVarName   :: DepVarName -- name of the dependent variable
        , _irodvEffN         :: Double     -- effective number of samples
        , _irodvWeightedAvg  :: Double     -- weighted average
        , _irodvWeightedVar  :: Double     -- weighted variance
        , _irodvLowerBound   :: Double     -- lower boundary of the 95% interval
        , _irodvMedian       :: Double     -- median
        , _irodvUpperBound   :: Double     -- upper boundary of the 95% interval
        , _irodvProbability  :: Maybe Double  -- Probability for search value
    } deriving (Eq, Show, Generic)

instance NFData InterpolationResultOneDepVar
instance Csv.DefaultOrdered InterpolationResultOneDepVar where
    headerOrder (InterpolationResultOneDepVar n _ _ _ _ _ _ (Just _)) =
        Csv.header $ map Bchs.pack [n ++ "EffN", n ++ "Avg", n ++ "Var", n ++ "Low", n ++ "Median", n ++ "Up", n ++ "Prob"]
    headerOrder (InterpolationResultOneDepVar n _ _ _ _ _ _ Nothing) =
        Csv.header $ map Bchs.pack [n ++ "EffN", n ++ "Avg", n ++ "Var", n ++ "Low", n ++ "Median", n ++ "Up"]
instance Csv.ToRecord InterpolationResultOneDepVar where
    toRecord (InterpolationResultOneDepVar _ neff a v lb m ub (Just p)) =
        Csv.record [
            Csv.toField neff, Csv.toField a, Csv.toField v, Csv.toField lb, Csv.toField m, Csv.toField ub, Csv.toField p
        ]
    toRecord (InterpolationResultOneDepVar _ neff a v lb m ub Nothing) =
        Csv.record [
            Csv.toField neff, Csv.toField a, Csv.toField v, Csv.toField lb, Csv.toField m, Csv.toField ub
        ]

-- | A datatype for dependent vars
newtype DepVarsPos = DepVarsPos [(DepVarName, Double)]
    deriving (Eq, Show, Generic)

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
}

-- | A datatype for spatio-temporal positions
data SpatTempPos = SpatTempPos {
      _spatialPos  :: SpatPos
    , _temporalPos :: TempPos
} deriving (Eq, Show, Generic)

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

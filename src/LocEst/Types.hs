-- {-# LANGUAGE StrictData #-}
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module LocEst.Types where

import           Control.Applicative   (empty)
import           Control.DeepSeq
import qualified Data.ByteString.Char8 as Bchs
import qualified Data.Csv              as Csv
import qualified Data.HashMap.Strict   as HM
import           Data.List             (sort, sortBy)
import           Data.Ord              (comparing)
import           GHC.Generics          (Generic)
import qualified Data.Vector as V

-- helper functions
filterLookup :: Csv.FromField a => Csv.NamedRecord -> Bchs.ByteString -> Csv.Parser a
filterLookup m name = maybe empty Csv.parseField $ HM.lookup name m

-- | A datatype for an unidirectional distance matrix
newtype SpatDistMap = SpatDistMatrixMap {
    _spatDistMatrixMap :: HM.HashMap (String, String) Double
} deriving (Show)

makeSpatDistMap :: [SpatDistObsGrid] -> SpatDistMap
makeSpatDistMap xs =
    SpatDistMatrixMap $ HM.fromList (map (\(SpatDistObsGrid oID gID d) -> ((oID,gID),d)) xs)

data SpatDistObsGrid = SpatDistObsGrid {
      _spatDistObsGridObsID :: String
    , _spatDistObsGridGridID :: String
    , _spatDistObsGridDistance :: Double
} deriving (Show, Generic)

instance NFData SpatDistObsGrid
instance Csv.FromNamedRecord SpatDistObsGrid where
    parseNamedRecord m = do
        SpatDistObsGrid <$> filterLookup m "obsID" <*> filterLookup m "gridID" <*> filterLookup m "dist"

-- | A datatype for crossvalidation output
data CrossvalOutput = CrossvalOutput {
      _crossoutDecayDef :: DecayDefinition
    , _crossoutSumAlgo  :: DensitySummaryAlgorithm
    , _crossoutProbSum  :: Double
} deriving (Show, Generic)

instance NFData CrossvalOutput
-- these instances are a quick hack - should actually be defined down to the algo types:
instance Csv.DefaultOrdered CrossvalOutput where
    headerOrder (CrossvalOutput decayDef sumAlg sumProb) =
        Csv.header ["decayDef"] <> Csv.header ["sumAlg"] <> Csv.header ["probability"]
instance Csv.ToRecord CrossvalOutput where
    toRecord (CrossvalOutput decayDef sumAlg sumProb) =
        Csv.record [Csv.toField (show decayDef)] <> Csv.record [Csv.toField (show sumAlg)] <> Csv.record [Csv.toField sumProb]

-- | A datatype for search result points in space and time
data SpatTempProb = SpatTempProb {
      _stprSpatTempDepVarsPosWithAlgos :: SpatTempDepVarsPosWithAlgorithms
    , _stprprobability :: Double
    -- to model the different densities per input point
    -- (which will certainly be necessary for debugging)
    -- SpatTempProb must somehow include also the source Observation
    -- Perhabs this could be implemented as a Maybe String for the Obs name?
} deriving (Show, Generic)

instance NFData SpatTempProb
instance Csv.DefaultOrdered SpatTempProb where
    headerOrder (SpatTempProb spatTempDepVarsPos prob) =
        Csv.headerOrder spatTempDepVarsPos <> Csv.header ["probability"]
instance Csv.ToRecord SpatTempProb where
    toRecord (SpatTempProb spatTempDepVarsPos prob) =
        Csv.toRecord spatTempDepVarsPos <> Csv.record [Csv.toField prob]

-- | A datatype that then also includes algorithms for a given point
data SpatTempDepVarsPosWithAlgorithms = SpatTempDepVarsPosWithAlgorithms {
      _powialgPosition    :: SpatTempDepVarsPos
    , _powialgDecayDef    :: DecayDefinition
    , _powialgDensSumAlgo :: DensitySummaryAlgorithm
} deriving (Show, Generic)

instance NFData SpatTempDepVarsPosWithAlgorithms
-- these instances are a quick hack - should actually be defined down to the algo types:
instance Csv.DefaultOrdered SpatTempDepVarsPosWithAlgorithms where
    headerOrder (SpatTempDepVarsPosWithAlgorithms spatTempDepVarsPos decayDef sumAlg) =
        Csv.headerOrder spatTempDepVarsPos <> Csv.header ["decayDef"] <> Csv.header ["sumAlg"]
instance Csv.ToRecord SpatTempDepVarsPosWithAlgorithms where
    toRecord (SpatTempDepVarsPosWithAlgorithms spatTempDepVarsPos decayDef sumAlg) =
        Csv.toRecord spatTempDepVarsPos <> Csv.record [Csv.toField (show decayDef)] <> Csv.record [Csv.toField (show sumAlg)]

-- Data types for core algorithm specification
data DensitySummaryAlgorithm =
      Maximum
    | Mean
    | DistanceWeightedMean
    deriving (Show, Eq, Ord, Generic)

instance NFData DensitySummaryAlgorithm

newtype DecayDefinition = DecayDefinition [DecayOneDepVar]
    deriving (Show, Eq, Ord, Generic)

instance NFData DecayDefinition

data DecayOneDepVar = DecayOneDepVar {
      _stddvDepVarName    :: DepVarName
    , _stddvSpatTempDecay :: DecayAlgorithm
    }
    deriving (Show, Eq, Ord, Generic)

instance NFData DecayOneDepVar

type DepVarName = String

data DecayAlgorithm =
      LinearSum Double Double
    | LogSum Double Double
    deriving (Show, Eq, Ord, Generic)

instance NFData DecayAlgorithm

-- | A datatype for observations in space and time also with coordinates in dependent var space
data SpatTempDepVarsPos = SpatTempDepVarsPos {
      _stpoSpatTempPos :: SpatTempPos
    , _stpoDepVarsPos  :: DepVarsPos
} deriving (Show, Generic)

instance NFData SpatTempDepVarsPos
instance Csv.FromNamedRecord SpatTempDepVarsPos where
    parseNamedRecord m = do
        spatTempPos <- Csv.parseNamedRecord m
        depVarsPos <- Csv.parseNamedRecord m
        pure $ SpatTempDepVarsPos {
              _stpoSpatTempPos = spatTempPos
            , _stpoDepVarsPos  = depVarsPos
            }
instance Csv.DefaultOrdered SpatTempDepVarsPos where
    headerOrder (SpatTempDepVarsPos spatTempPos depVarsPos) =
        Csv.headerOrder spatTempPos <> Csv.headerOrder depVarsPos
instance Csv.ToRecord SpatTempDepVarsPos where
    toRecord (SpatTempDepVarsPos spatTempPos depVarsPos) =
        Csv.toRecord spatTempPos <> Csv.toRecord depVarsPos

-- | A datatype for dependent vars
newtype DepVarsPos = DepVarsPos { getHM :: HM.HashMap String Double }
    deriving (Show, Generic)

instance NFData DepVarsPos
instance Csv.FromNamedRecord DepVarsPos where
    parseNamedRecord m = do
        let extractedVarsBS = HM.filterWithKey (\k _ -> Bchs.isPrefixOf "var" k) m
        let extractedVarsStringDouble = HM.mapKeys Bchs.unpack $ HM.map (read . Bchs.unpack) $ extractedVarsBS
        pure $ DepVarsPos extractedVarsStringDouble
instance Csv.DefaultOrdered DepVarsPos where
    headerOrder (DepVarsPos hm) =
        V.map Bchs.pack $ V.fromList $ sort $ map fst $ HM.toList hm
instance Csv.ToRecord DepVarsPos where
    toRecord (DepVarsPos hm) =
        let orderedValues = map snd $ sortBy (\(k1,_) (k2,_) -> compare k1 k2) $ HM.toList $ hm
        in V.map (Bchs.pack . show) $ V.fromList orderedValues

depVarsExtractOrdered :: [String] -> DepVarsPos -> [Double]
depVarsExtractOrdered orderedKeys (DepVarsPos hm) =
    map (hm HM.!) orderedKeys

-- | A datatype for distances in space and time
data SpatTempDist = SpatTempDist {
      _spatDist :: Double
    , _tempDist :: Double
}

-- | A datatype for spatio-temporal positions
data SpatTempPos = SpatTempPos {
      _spatialPos  :: SpatPos
    , _temporalPos :: TempPos
} deriving (Show, Generic)

instance NFData SpatTempPos
instance Csv.FromNamedRecord SpatTempPos where
    parseNamedRecord m = do
        spatPos <- Csv.parseNamedRecord m
        tempPos <- SimpleYearBCAD <$> filterLookup m "age"
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

-- | A datatype for temporal positions
data TempPos =
    SimpleYearBCAD YearBCAD -- TODO: add more complex models
    deriving (Show, Generic)

instance NFData TempPos
instance Csv.DefaultOrdered TempPos where
    headerOrder (SimpleYearBCAD x) = Csv.header ["age"]
instance Csv.ToField TempPos where
    toField (SimpleYearBCAD x) = Csv.toField x

type YearBP = Word
type YearBCAD = Int
type YearRange = Word

-- | A datatype for spatial positions
data SpatPos = SpatPosCartesian CartesianPos | SpatPosLongLat LongLatPos
    deriving (Show, Generic)

instance NFData SpatPos
instance Csv.FromNamedRecord SpatPos where
    parseNamedRecord m = do
        SpatPosCartesian <$> (CartesianPos <$> filterLookup m "x" <*> filterLookup m "y")
instance Csv.DefaultOrdered SpatPos where
    headerOrder (SpatPosCartesian x) = Csv.headerOrder x
    headerOrder (SpatPosLongLat x)   = Csv.headerOrder x
instance Csv.ToRecord SpatPos where
    toRecord (SpatPosCartesian x) = Csv.toRecord x
    toRecord (SpatPosLongLat x)   = Csv.toRecord x

-- | A datatype for projected coordinates
data CartesianPos = CartesianPos Double Double
    deriving (Show, Generic)

instance NFData CartesianPos
instance Csv.DefaultOrdered CartesianPos where
    headerOrder _ = Csv.header ["x", "y"]
instance Csv.ToRecord CartesianPos where
    toRecord (CartesianPos x y) = Csv.record [Csv.toField x, Csv.toField y]

-- | A datatype for Long-Lat coordinates
data LongLatPos = LongLatPos Longitude Latitude
    deriving (Show, Generic)

instance NFData LongLatPos
instance Csv.DefaultOrdered LongLatPos where
    headerOrder _ = Csv.header ["longitude", "latitude"]
instance Csv.ToRecord LongLatPos where
    toRecord (LongLatPos long lat) = Csv.record [Csv.toField long, Csv.toField lat]

-- | A datatype for Longitudes
newtype Longitude = Longitude Double
    deriving (Show, Generic)

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
    deriving (Show, Generic)

makeLatitude :: MonadFail m => Double -> m Latitude
makeLatitude x
    | x >= -90 && x <= 90 = pure (Latitude x)
    | otherwise           = fail $ "Latitude " ++ show x ++ " not between -90 and 90"

instance NFData Latitude
instance Csv.ToField Latitude where
    toField (Latitude x) = Csv.toField x
instance Csv.FromField Latitude where
    parseField x = Csv.parseField x >>= makeLatitude

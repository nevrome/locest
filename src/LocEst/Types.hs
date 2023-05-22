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
import           Data.List             (sortBy)
import           Data.Ord              (comparing)
import           GHC.Generics          (Generic)
import qualified Data.Vector as V

-- helper functions
filterLookup :: Csv.FromField a => Csv.NamedRecord -> Bchs.ByteString -> Csv.Parser a
filterLookup m name = maybe empty Csv.parseField $ HM.lookup name m

-- | A datatype for distances in space and time
data SpatTempDist = SpatTempDist {
      _spatDist :: Double
    , _tempDist :: Double
}

-- | A datatype for search result points in space and time
data SpatTempProb = SpatTempProb {
      _stprspatTempPos :: SpatTempPos
    , _stprDepVarsPos  :: DepVarsPos
    , _stprprobability :: Double -- must be more complex to express various things, this is where different densities for different input points can go
} deriving (Show, Generic)

instance NFData SpatTempProb
instance Csv.ToRecord SpatTempProb where
    toRecord (SpatTempProb spatTempPos depVarsPos prob) = Csv.toRecord spatTempPos <> Csv.toRecord depVarsPos <> Csv.record [Csv.toField prob]

-- | A datatype for observations in space and time also with coordinates in dependent var space
data SpatTempDepVarsPos = SpatTempDepVarsPos {
      _stpoSpatTempPos :: SpatTempPos
    , _stpoDepVarsPos  :: DepVarsPos
} deriving Show

instance Csv.FromNamedRecord SpatTempDepVarsPos where
    parseNamedRecord m = do
        spatTempPos <- Csv.parseNamedRecord m
        depVarsPos <- Csv.parseNamedRecord m
        pure $ SpatTempDepVarsPos {
              _stpoSpatTempPos = spatTempPos
            , _stpoDepVarsPos  = depVarsPos
            }

multiplySpatPosByDepVarsPos :: [DepVarsPos] -> SpatTempPos -> [SpatTempDepVarsPos]
multiplySpatPosByDepVarsPos depVarsPos spatTempPos =
    map (\p -> SpatTempDepVarsPos { _stpoSpatTempPos = spatTempPos, _stpoDepVarsPos = p}) depVarsPos

-- | A datatype for dependent vars
newtype DepVarsPos = DepVarsPos { getHM :: HM.HashMap String Double }
    deriving (Show, Generic)

instance NFData DepVarsPos
instance Csv.FromNamedRecord DepVarsPos where
    parseNamedRecord m = do
        let extractedVarsBS = HM.filterWithKey (\k _ -> Bchs.isPrefixOf "var" k) m
        let extractedVarsStringDouble = HM.mapKeys Bchs.unpack $ HM.map (read . Bchs.unpack) $ extractedVarsBS
        pure $ DepVarsPos extractedVarsStringDouble
instance Csv.ToRecord DepVarsPos where
    toRecord (DepVarsPos hm) =
        let orderedValues = map snd $ sortBy (\(k1,_) (k2,_) -> compare k1 k2) $ HM.toList $ hm
        in V.map (Bchs.pack . show) $ V.fromList orderedValues

depVarsExtractOrdered :: [String] -> DepVarsPos -> [Double]
depVarsExtractOrdered orderedKeys (DepVarsPos hm) =
    map (hm HM.!) orderedKeys

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
instance Csv.ToRecord SpatTempPos where
    toRecord (SpatTempPos spatPos tempPos) = Csv.toRecord spatPos <> Csv.record [Csv.toField tempPos]

multiplySpatPosByTempGrid :: [Int] -> SpatPos -> [SpatTempPos]
multiplySpatPosByTempGrid tempGrid spatPos =
    map (\y -> SpatTempPos { _spatialPos = spatPos, _temporalPos = SimpleYearBCAD y}) tempGrid

-- | A datatype for temporal positions
data TempPos =
    SimpleYearBCAD YearBCAD -- TODO: add more complex models
    deriving (Show, Generic)

instance NFData TempPos
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
instance Csv.ToRecord SpatPos where
    toRecord (SpatPosCartesian x) = Csv.toRecord x
    toRecord (SpatPosLongLat x)   = Csv.toRecord x

-- | A datatype for projected coordinates
data CartesianPos = CartesianPos Double Double
    deriving (Show, Generic)

instance NFData CartesianPos
instance Csv.ToRecord CartesianPos where
    toRecord (CartesianPos x y) = Csv.record [Csv.toField x, Csv.toField y]

-- | A datatype for Long-Lat coordinates
data LongLatPos = LongLatPos Longitude Latitude
    deriving (Show, Generic)

instance NFData LongLatPos
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

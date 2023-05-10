-- {-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ApplicativeDo #-}

module LocEst.Types where

import qualified Data.Csv                             as Csv
import qualified Data.ByteString.Char8                as Bchs
import qualified Data.HashMap.Strict                  as HM
import           Control.Applicative                  (empty)
import GHC.Generics (Generic)


-- helper functions
filterLookup :: Csv.FromField a => Csv.NamedRecord -> Bchs.ByteString -> Csv.Parser a
filterLookup m name = maybe empty Csv.parseField $ HM.lookup name m

-- | A datatype for distances in space and time
data SpatTempDist = SpatTempDist {
      _spatDist :: Double
    , _tempDist :: Double
}

-- | A datatype for result points in space and time
data SpatTempProb = SpatTempProb {
      _stprspatTempPos :: SpatTempPos
    , _stprprobability :: Double
} deriving Show

instance Csv.ToRecord SpatTempProb where
    toRecord (SpatTempProb spatTempPos prob) = Csv.toRecord spatTempPos <> Csv.record [Csv.toField prob]

-- | A datatype for observations in space and time
data SpatTempObs = SpatTempObs {
      _stpoSpatTempPos :: SpatTempPos
    , _stpopc1         :: Double -- TODO: add a data structure to store
                                 -- more variables, maybe a Map
} deriving Show

instance Csv.FromNamedRecord SpatTempObs where
    parseNamedRecord m = do
        spatTempPos <- Csv.parseNamedRecord m
        pc1 <- filterLookup m "pc1"
        pure $ SpatTempObs {
              _stpoSpatTempPos = spatTempPos
            , _stpopc1         = pc1
            }

-- | A datatype for spatio-temporal positions
data SpatTempPos = SpatTempPos {
      _spatialPos  :: SpatPos
    , _temporalPos :: TempPos
} deriving (Show, Generic)

instance Csv.FromNamedRecord SpatTempPos where
    parseNamedRecord m = do
        spatPos <- SpatPosCartesian <$> (CartesianPos <$> filterLookup m "x" <*> filterLookup m "y")
        tempPos <- SimpleYearBCAD <$> filterLookup m "age"
        pure $ SpatTempPos {
              _spatialPos = spatPos
            , _temporalPos = tempPos
            }

instance Csv.ToRecord SpatTempPos where
    toRecord (SpatTempPos spatPos tempPos) = Csv.toRecord spatPos <> Csv.record [Csv.toField tempPos]

-- | A datatype for temporal positions
data TempPos =
    SimpleYearBCAD YearBCAD -- TODO: add more complex models
    deriving (Show, Generic)

instance Csv.ToField TempPos where
    toField (SimpleYearBCAD x) = Csv.toField x

type YearBP = Word
type YearBCAD = Int
type YearRange = Word

-- | A datatype for spatial positions
data SpatPos = SpatPosCartesian CartesianPos | SpatPosLongLat LongLatPos
    deriving (Show, Generic)

instance Csv.ToRecord SpatPos where
    toRecord (SpatPosCartesian x) = Csv.toRecord x
    toRecord (SpatPosLongLat x)   = Csv.toRecord x

-- | A datatype for projected coordinates
data CartesianPos = CartesianPos Double Double
    deriving (Show)

instance Csv.ToRecord CartesianPos where
    toRecord (CartesianPos x y) = Csv.record [Csv.toField x, Csv.toField y]

-- | A datatype for Long-Lat coordinates
data LongLatPos = LongLatPos Longitude Latitude
    deriving (Show)

instance Csv.ToRecord LongLatPos where
    toRecord (LongLatPos long lat) = Csv.record [Csv.toField long, Csv.toField lat]

-- | A datatype for Longitudes
newtype Longitude = Longitude Double
    deriving (Show)

makeLongitude :: MonadFail m => Double -> m Longitude
makeLongitude x
    | x >= -180 && x <= 180 = pure (Longitude x)
    | otherwise             = fail $ "Longitude " ++ show x ++ " not between -180 and 180"

instance Csv.ToField Longitude where
    toField (Longitude x) = Csv.toField x
instance Csv.FromField Longitude where
    parseField x = Csv.parseField x >>= makeLongitude

-- | A datatype for Latitudes
newtype Latitude = Latitude Double
    deriving (Show)

makeLatitude :: MonadFail m => Double -> m Latitude
makeLatitude x
    | x >= -90 && x <= 90 = pure (Latitude x)
    | otherwise           = fail $ "Latitude " ++ show x ++ " not between -90 and 90"


instance Csv.ToField Latitude where
    toField (Latitude x) = Csv.toField x
instance Csv.FromField Latitude where
    parseField x = Csv.parseField x >>= makeLatitude
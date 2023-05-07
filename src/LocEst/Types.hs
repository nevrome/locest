-- {-# LANGUAGE StrictData #-}

module LocEst.Types where

import qualified Data.Csv                             as Csv

-- | A datatype for observations in space and time
data SpatTempObs = SpatTempObs {
      _spatTempPos :: SpatTempPos
    , _pc1         :: Double -- TODO: add a data structure to store
                             -- more variables, maybe a Map
}

-- | A datatype for spatio-temporal positions
data SpatTempPos = SpatTempPos {
      _spatialPos  :: SpatPos
    , _temporalPos :: TempPos
}

-- | A datatype for temporal positions
data TempPos =
    SimpleYearBCAD YearBCAD -- TODO: add more complex models

type YearBP = Word
type YearBCAD = Int
type YearRange = Word

-- | A datatype for spatial positions
data SpatPos = SpatPosCartesian CartesianPos | SpatPosLongLat LongLatPos
    deriving (Show)

-- | A datatype for projected coordinates
data CartesianPos = CartesianPos Double Double
    deriving (Show)

makeCartesianPos :: MonadFail m => Double -> Double -> m CartesianPos
makeCartesianPos x y = do
    return $ CartesianPos x y

-- | A datatype for Long-Lat coordinates
data LongLatPos = LongLatPos Longitude Latitude
    deriving (Show)

makeLongLatPos :: MonadFail m => Double -> Double -> m LongLatPos
makeLongLatPos long lat = do
    longitude <- makeLongitude long
    latitude <- makeLatitude lat
    return $ LongLatPos longitude latitude

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
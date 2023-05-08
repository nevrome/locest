-- {-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings   #-}

module LocEst.Types where

import qualified Data.Csv                             as Csv
import qualified Data.ByteString.Char8                as Bchs
import qualified Data.HashMap.Strict                  as HM
import           Control.Applicative                  (empty)


-- helper functions
filterLookup :: Csv.FromField a => Csv.NamedRecord -> Bchs.ByteString -> Csv.Parser a
filterLookup m name = maybe empty Csv.parseField $ HM.lookup name m



-- | A datatype for raw tsv SpatTempObs input
data SpatTempObsTsvRow = SpatTempObsTsvRow {
      _stotID :: String
    , _stotX  :: Double
    , _stotY  :: Double
    , _stotSimpleAge :: YearBCAD
    , _stotPC1 :: Double
} deriving Show

instance Csv.FromNamedRecord SpatTempObsTsvRow where
    parseNamedRecord m = SpatTempObsTsvRow
        <$> filterLookup m "id"
        <*> filterLookup m "x"
        <*> filterLookup m "y"
        <*> filterLookup m "age"
        <*> filterLookup m "pc1"

-- | A datatype for observations in space and time
data SpatTempObs = SpatTempObs {
      _spatTempPos :: SpatTempPos
    , _pc1         :: Double -- TODO: add a data structure to store
                             -- more variables, maybe a Map
} deriving Show

-- | A datatype for spatio-temporal positions
data SpatTempPos = SpatTempPos {
      _spatialPos  :: SpatPos
    , _temporalPos :: TempPos
} deriving Show

-- | A datatype for temporal positions
data TempPos =
    SimpleYearBCAD YearBCAD -- TODO: add more complex models
    deriving Show

type YearBP = Word
type YearBCAD = Int
type YearRange = Word

-- | A datatype for spatial positions
data SpatPos = SpatPosCartesian CartesianPos | SpatPosLongLat LongLatPos
    deriving (Show)

-- | A datatype for projected coordinates
data CartesianPos = CartesianPos Double Double
    deriving (Show)

-- | A datatype for Long-Lat coordinates
data LongLatPos = LongLatPos Longitude Latitude
    deriving (Show)

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
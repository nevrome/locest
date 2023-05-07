-- {-# LANGUAGE StrictData #-}

module LocEst.Types where

data SpatTempObs = SpatTempObs {
      _spatTempPos :: SpatTempPos
    , _pc1         :: Double -- TODO: add a data structure to store
                             -- more variables, maybe a Map
}

data SpatTempPos = SpatTempPos {
      _spatialPos  :: SpatPos
    , _temporalPos :: TempPos
}

data TempPos =
    SimpleYearBCAD YearBCAD -- TODO: add more complex models

type YearBP = Word
type YearBCAD = Int
type YearRange = Word

data SpatPos =
      LongLat Longitude Latitude
    | CartesianCoord Double Double
    deriving (Show)

newtype Longitude = Longitude Double
    deriving (Show)
newtype Latitude = Latitude Double
    deriving (Show)



{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module LocEst.Types where

import           Control.Applicative   (empty, (<|>))
import           Control.DeepSeq
import qualified Data.ByteString.Char8 as Bchs
import qualified Data.ByteString.Short as BSS
import qualified Data.Csv              as Csv
import qualified Data.HashMap.Strict   as HM
import           Data.List             (nub, sort, sortBy)
import           Data.String           (fromString)
import qualified Data.Vector           as V
import           GHC.Generics          (Generic)
import           LocEst.Utils          (LOCESTException (..))

-- helper functions
filterLookup :: Csv.FromField a => Csv.NamedRecord -> Bchs.ByteString -> Csv.Parser a
filterLookup m name = maybe empty Csv.parseField $ HM.lookup name m

filterLookupOptional :: Csv.FromField a => Csv.NamedRecord -> Bchs.ByteString -> Csv.Parser (Maybe a)
filterLookupOptional m name = maybe (pure Nothing) Csv.parseField $ HM.lookup name m

-- | A data type to represent setting permutations
data PermutationTree =
      PTLeaf PositionEntity
    | PTFork PositionEntity [PermutationTree]
    | PTRoot [PermutationTree]

addPermutation :: [PositionEntity] -> PermutationTree -> PermutationTree
addPermutation [] t             = t
addPermutation xs (PTLeaf v)    = PTFork v (map PTLeaf xs)
addPermutation xs (PTRoot [])   = PTRoot (map PTLeaf xs)
addPermutation xs (PTRoot ts)   = PTRoot (map (\t -> addPermutation xs t) ts)
addPermutation xs (PTFork v ts) = PTFork v (map (\t -> addPermutation xs t) ts)

harvest :: PermutationTree -> Either LOCESTException [SpatTempDepVarsPosWithAlgorithms]
harvest = harvestFlattened . flattenTree
    where
        flattenTree :: PermutationTree -> [[PositionEntity]]
        flattenTree (PTRoot ts)   = concatMap flattenTree ts
        flattenTree (PTFork v ts) = map (v:) (concatMap flattenTree ts)
        flattenTree (PTLeaf v)    = [[v]]
        harvestFlattened :: [[PositionEntity]] -> Either LOCESTException [SpatTempDepVarsPosWithAlgorithms]
        harvestFlattened = mapM pluckOne
            where
                pluckOne :: [PositionEntity] -> Either LOCESTException SpatTempDepVarsPosWithAlgorithms
                pluckOne xs = do
                    spatPos    <- exactlyOnce [ v | PESpatPos v <- xs]
                    tempPos    <- exactlyOnce [ v | PETempPos v <- xs]
                    depVarsPos <- exactlyOnce [ v | PEDepVarsPos v <- xs]
                    algorithm  <- exactlyOnce [ v | PEAlgorithm v <- xs]
                    return $
                        SpatTempDepVarsPosWithAlgorithms
                            (SpatTempDepVarsPos (SpatTempPos spatPos (SimpleYearBCAD tempPos)) depVarsPos)
                            algorithm
                    where
                        exactlyOnce :: Eq a => [a] -> Either LOCESTException a
                        exactlyOnce es =
                            if length (nub es) == 1
                            then Right $ head es
                            else Left $ NormalException "Permutation tree inconsistent"

data PositionEntity =
      PESpatPos SpatPos
    | PETempPos Int
    | PEDepVarsPos DepVarsPos
    | PEAlgorithm LocestAlgorithm
    deriving (Eq)

-- a typeclass for things with ids
class Identifiable a where
    getID :: a -> String

-- | A datatype for an unidirectional distance matrix
newtype SpatDistMap = SpatDistMatrixMap {
    _spatDistMatrixMap :: HM.HashMap (BSS.ShortByteString, BSS.ShortByteString) Double
} deriving (Show, Generic)

makeSpatDistMap :: [SpatDistObsGrid] -> SpatDistMap
makeSpatDistMap xs =
    SpatDistMatrixMap $ HM.fromList (map (\(SpatDistObsGrid oID gID d) -> ((fromString oID, fromString gID),d)) xs)

instance NFData SpatDistMap

data SpatDistObsGrid = SpatDistObsGrid {
      _spatDistObsGridObsID    :: String
    , _spatDistObsGridGridID   :: String
    , _spatDistObsGridDistance :: Double
} deriving (Show, Generic)

instance NFData SpatDistObsGrid
instance Csv.FromNamedRecord SpatDistObsGrid where
    parseNamedRecord m =
        SpatDistObsGrid <$> filterLookup m "obsID" <*> filterLookup m "spatID" <*> filterLookup m "dist"

-- | A datatype for crossvalidation output
data CrossvalOutput = CrossvalOutput {
      _crossoutAlgorithm :: LocestAlgorithm
    , _crossoutProbSum   :: Double
} deriving (Show, Generic)

instance NFData CrossvalOutput
-- these instances are a quick hack - should actually be defined down to the algo types:
instance Csv.DefaultOrdered CrossvalOutput where
    headerOrder (CrossvalOutput algo _) =
        Csv.headerOrder algo <> Csv.header ["probability"]
instance Csv.ToRecord CrossvalOutput where
    toRecord (CrossvalOutput algo sumProb) =
        Csv.toRecord algo <> Csv.record [Csv.toField sumProb]

-- | A datatype for search result points in space and time
data SearchResult = SearchResult {
      _srSpatTempDepVarsPosWithAlgos :: SpatTempDepVarsPosWithAlgorithms
    , _srInterpolation               :: Maybe DepVarsUncertainPos
    , _srProbability                 :: Double
    -- to model the different densities per input point
    -- (which will certainly be necessary for debugging)
    -- SpatTempProb must somehow include also the source Observation
    -- Perhabs this could be implemented as a Maybe String for the Obs name?
} deriving (Show, Generic)

instance NFData SearchResult
instance Csv.DefaultOrdered SearchResult where
    headerOrder (SearchResult spatTempDepVarsPos Nothing _) =
        Csv.headerOrder spatTempDepVarsPos <> Csv.header ["probability"]
    headerOrder (SearchResult spatTempDepVarsPos (Just depVarsUncertainPos) _) =
        Csv.headerOrder spatTempDepVarsPos <> Csv.headerOrder depVarsUncertainPos <> Csv.header ["probability"]
instance Csv.ToRecord SearchResult where
    toRecord (SearchResult spatTempDepVarsPos Nothing prob) =
        Csv.toRecord spatTempDepVarsPos <> Csv.record [Csv.toField prob]
    toRecord (SearchResult spatTempDepVarsPos (Just depVarsUncertainPos) prob) =
        Csv.toRecord spatTempDepVarsPos <> Csv.toRecord depVarsUncertainPos <> Csv.record [Csv.toField prob]

data SpatTempProb = SpatTempProb {
      _stprSpatTempDepVarsPosWithAlgos :: SpatTempDepVarsPosWithAlgorithms
    , _stprprobability                 :: Double
    -- to model the different densities per input point
    -- (which will certainly be necessary for debugging)
    -- SpatTempProb must somehow include also the source Observation
    -- Perhabs this could be implemented as a Maybe String for the Obs name?
} deriving (Show, Generic)

instance NFData SpatTempProb
instance Csv.DefaultOrdered SpatTempProb where
    headerOrder (SpatTempProb spatTempDepVarsPos _) =
        Csv.headerOrder spatTempDepVarsPos <> Csv.header ["probability"]
instance Csv.ToRecord SpatTempProb where
    toRecord (SpatTempProb spatTempDepVarsPos prob) =
        Csv.toRecord spatTempDepVarsPos <> Csv.record [Csv.toField prob]

-- | A datatype that then also includes algorithms for a given point
data SpatTempDepVarsPosWithAlgorithms = SpatTempDepVarsPosWithAlgorithms {
      _powialgPosition  :: SpatTempDepVarsPos
    , _powialgAlgorithm :: LocestAlgorithm
} deriving (Show, Generic)

instance NFData SpatTempDepVarsPosWithAlgorithms
instance Csv.DefaultOrdered SpatTempDepVarsPosWithAlgorithms where
    headerOrder (SpatTempDepVarsPosWithAlgorithms spatTempDepVarsPos algorithm) =
        Csv.headerOrder spatTempDepVarsPos <> Csv.headerOrder algorithm
instance Csv.ToRecord SpatTempDepVarsPosWithAlgorithms where
    toRecord (SpatTempDepVarsPosWithAlgorithms spatTempDepVarsPos algorithm) =
        Csv.toRecord spatTempDepVarsPos <> Csv.toRecord algorithm

-- Data types for core algorithm specification
data LocestAlgorithm =
    AlgoSepIDW {
        _asiDecayDefinition :: DecayDefinition
      , _asiDensitySummary  :: DensitySummaryAlgorithm
    } |
    AlgoKernSmooth {
        _aksKernelDefinition :: KernelDefinition
    }
    deriving (Show, Eq, Ord, Generic)

instance NFData LocestAlgorithm
-- these instances are just placeholders
instance Csv.DefaultOrdered LocestAlgorithm where
    headerOrder (AlgoSepIDW _ _) =
        Csv.header ["decayDef"] <> Csv.header ["sumAlg"]
    headerOrder (AlgoKernSmooth _) =
        Csv.header ["kernDef"]
instance Csv.ToRecord LocestAlgorithm where
    toRecord (AlgoSepIDW decayDef sumAlg) =
        Csv.record [Csv.toField (show decayDef)] <> Csv.record [Csv.toField (show sumAlg)]
    toRecord (AlgoKernSmooth kernDef) =
        Csv.record [Csv.toField (show kernDef)]

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

newtype KernelDefinition = KernelDefinition [KernelOneDepVar]
    deriving (Show, Eq, Ord, Generic)

instance NFData KernelDefinition

data KernelOneDepVar = KernelOneDepVar {
      _kodvDepVarName :: DepVarName
    , _kodvKernel     :: Kernel
    }
    deriving (Show, Eq, Ord, Generic)

instance NFData KernelOneDepVar

data Kernel =
      Uniform Double Double
    | Normal Double Double
    deriving (Show, Eq, Ord, Generic)

instance NFData Kernel

data ObsWithDist = ObsWithDist {
      _owdObservation  :: Observation
    , _owdSpatTempDist :: SpatTempDist
}

addDistsToObs :: Observation -> Double -> Double -> ObsWithDist
addDistsToObs obs spatDist tempDist = ObsWithDist obs (SpatTempDist spatDist tempDist)

-- | A datatype for observations with id and position
data Observation = Observation {
      _obsID  :: String
    , _obsPos :: SpatTempDepVarsPos
} deriving (Show, Generic)

instance NFData Observation
instance Csv.FromNamedRecord Observation where
    parseNamedRecord m = do
        identifier <- filterLookup m "obsID"
        position <- Csv.parseNamedRecord m
        pure $ Observation {
              _obsID = identifier
            , _obsPos = position
            }
instance Csv.DefaultOrdered Observation where
    headerOrder (Observation _ position) =
        Csv.header ["obsID"] <> Csv.headerOrder position
instance Csv.ToRecord Observation where
    toRecord (Observation identifier position) =
        Csv.toRecord identifier <> Csv.toRecord position
instance Identifiable Observation where
    getID (Observation identifier _) = identifier

-- | A datatype for positions in space, time and in dependent var space
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

-- | A datatype for dependent vars with errors
newtype DepVarsUncertainPos = DepVarsUncertainPos { _dvupGetHM :: HM.HashMap String (Double, Double, Double) }
    deriving (Eq, Show, Generic)

instance NFData DepVarsUncertainPos
instance Csv.DefaultOrdered DepVarsUncertainPos where
    headerOrder (DepVarsUncertainPos hm) =
        V.map Bchs.pack $ V.fromList $ concatMap (\n -> [n ++ "Res", n ++ "ResErr", n ++ "Dens"]) $ sort $ map fst $ HM.toList hm
instance Csv.ToRecord DepVarsUncertainPos where
    toRecord (DepVarsUncertainPos hm) =
        let orderedValues = map snd $ sortBy (\(k1,_) (k2,_) -> compare k1 k2) $ HM.toList $ hm
        in V.map (Bchs.pack . show) $ V.fromList $ concatMap (\(a,b,c) -> [a,b,c]) orderedValues

-- | A datatype for dependent vars
newtype DepVarsPos = DepVarsPos { getHM :: HM.HashMap String Double }
    deriving (Eq, Show, Generic)

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
    deriving (Eq, Show, Generic)

instance NFData TempPos
instance Csv.DefaultOrdered TempPos where
    headerOrder (SimpleYearBCAD _) = Csv.header ["age"]
instance Csv.ToField TempPos where
    toField (SimpleYearBCAD x) = Csv.toField x

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

-- | A datatype for projected coordinates
data CartesianPos = CartesianPos (Maybe String) Double Double
    deriving (Eq, Show, Generic)

instance NFData CartesianPos
instance Csv.FromNamedRecord CartesianPos where
    parseNamedRecord m =
        CartesianPos <$> filterLookupOptional m "spatID" <*> filterLookup m "x" <*> filterLookup m "y"
instance Csv.DefaultOrdered CartesianPos where
    headerOrder _ = Csv.header ["spatID", "x", "y"]
instance Csv.ToRecord CartesianPos where
    toRecord (CartesianPos s x y) = Csv.record [Csv.toField s, Csv.toField x, Csv.toField y]
instance Identifiable CartesianPos where
    getID (CartesianPos Nothing _ _)           = "unnamed"
    getID (CartesianPos (Just identifier) _ _) = identifier

-- | A datatype for Long-Lat coordinates
data LongLatPos = LongLatPos (Maybe String) Longitude Latitude
    deriving (Eq, Show, Generic)

instance NFData LongLatPos
instance Csv.FromNamedRecord LongLatPos where
    parseNamedRecord m =
        LongLatPos <$> filterLookupOptional m "spatID" <*> filterLookup m "longitude" <*> filterLookup m "latitude"
instance Csv.DefaultOrdered LongLatPos where
    headerOrder _ = Csv.header ["spatID", "longitude", "latitude"]
instance Csv.ToRecord LongLatPos where
    toRecord (LongLatPos s long lat) = Csv.record [Csv.toField s, Csv.toField long, Csv.toField lat]
instance Identifiable LongLatPos where
    getID (LongLatPos Nothing _ _)           = "unnamed"
    getID (LongLatPos (Just identifier) _ _) = identifier

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
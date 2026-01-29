{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE StrictData             #-}

-- rexport of LocEst.TypesUtils
module LocEst.Types (module LocEst.Types, module LocEst.TypesUtils) where

import           LocEst.TypesUtils
import           LocEst.Utils          (throwL)

import qualified Codec.Serialise       as S
import           Control.Applicative   ((<|>))
import           Control.DeepSeq
import qualified Data.ByteString.Char8 as Bchs
import qualified Data.Csv              as Csv
import qualified Data.HashMap.Strict   as HM
import           Data.List             (find, sortBy)
import           Data.Maybe            (isNothing)
import qualified Data.Vector           as V
import qualified Data.Vector.Storable  as VS
import           GHC.Generics          (Generic)

-- filtering and sorting variables

-- filter variables
filterByKeyList :: Eq a => [String] -> [(String,a)] -> [(String,a)]
filterByKeyList keys = filter (\(k,_) -> k `elem` keys)

filterVarsInObs :: [String] -> [String] -> V.Vector Observation -> V.Vector Observation
filterVarsInObs depVarsWanted indepVarsWanted = V.map handleOne
    where
    handleOne :: Observation -> Observation
    -- spatiotemporal case
    handleOne o@(Observation _ _ (HyperPos std@(IndepSpatTempPos _) depInObs) _) =
        let depRes = filterByKey depVarsWanted depInObs
        in o { _obsPos = HyperPos std depRes }
    -- arbitrary dimension case
    handleOne o@(Observation _ _ (HyperPos (IndepArbitraryDimPos indepInObs) depInObs) _) =
        let depRes   = filterByKey depVarsWanted depInObs
            indepRes = filterByKey indepVarsWanted indepInObs
        in o { _obsPos = HyperPos (IndepArbitraryDimPos indepRes) depRes }

filterVarsInIndepVarsPos :: [String] -> IndepVarsPos -> IndepVarsPos
filterVarsInIndepVarsPos _ x@(IndepSpatTempPos _) = x
filterVarsInIndepVarsPos indepVarsWanted (IndepArbitraryDimPos x) =
    IndepArbitraryDimPos $ filterByKey indepVarsWanted x

-- data types

-- | A data type for interpolation output for one dependent variable
data SearchResultLong = SRL {
      _srlDepVarName        :: DepVarName -- name of the dependent variable
    , _srlLowerBound        :: Double     -- lower boundary of the 95% interval
    , _srlMedian            :: Double     -- median (weighted average)
    , _srlUpperBound        :: Double     -- upper boundary of the 95% interval
    , _srlGridDepPos        :: Maybe DepVarsPos
    , _srlGridLogLikelihood :: Maybe Double -- log-likelihood for true value
    , _srlSearchPos         :: Maybe (V.Vector DepVarsPredPos) -- search values
    , _srlLogLikelihood     :: Maybe (V.Vector Double) -- log-likelihood for search value
    , _srlTopObsIDs         :: Maybe String
} deriving (Eq, Show, Generic)

-- | A data type for interpolation output, aggregated per row (so per grid position and per search candidate)
data SearchResultWide = SRW {
      _srwTempSampIter      :: Int
    , _srwKernDef           :: KernelDefinition
    , _srwGridIndepVarsPos  :: IndepVarsPos
    , _srwTopObsIDs         :: [Maybe String]
    , _srwDepVarName        :: [DepVarName]
    , _srwLowerBound        :: [Double]
    , _srwMedian            :: [Double]
    , _srwUpperBound        :: [Double]
    , _srwGridLogLikelihood :: [Maybe Double]
    , _srwGridAggLogLik     :: Maybe Double
    , _srwSearchPos         :: Maybe DepVarsPredPos
    , _srwLogLikelihood     :: [Maybe Double]
    , _srwAggLogLikelihood  :: Maybe Double
    , _srwProbability       :: Maybe Double
} deriving (Eq, Show, Generic)

instance Csv.DefaultOrdered SearchResultWide where
    headerOrder (SRW _ kernDef gridIndep topObs names _ _ _ gridLLs gridAgg mSearch lls aggLLs probs) =
        let perDepCols :: DepVarName -> [Bchs.ByteString]
            perDepCols dv =
              map Bchs.pack (
                    [ "interpol_lower_quant_"  ++ dv
                    , "interpol_median_"       ++ dv
                    , "interpol_upper_quant_"  ++ dv
                    ]
                    ++ ["grid_top_obs_" ++ dv | not (all isNothing topObs)]
                    ++ ["grid_log_likelihood_" ++ dv | not (all isNothing gridLLs)]
                    ++ ["search_log_likelihood_" ++ dv | not (all isNothing lls)]
                )
        in    Csv.header ["temp_sampling_iteration"]
           <> Csv.headerOrder kernDef
           <> V.map ("grid_" <>) (Csv.headerOrder gridIndep)
           <> maybe V.empty Csv.headerOrder mSearch
           <> V.fromList (concatMap perDepCols names)
           <> maybe V.empty (const $ Csv.header ["grid_agg_log_likelihood"]) gridAgg
           <> maybe V.empty (const $ Csv.header ["search_agg_log_likelihood"]) aggLLs
           <> maybe V.empty (const $ Csv.header ["search_probability"]) probs
instance Csv.ToRecord SearchResultWide where
    toRecord (SRW tsi kernDef gridIndep topObs names lowB medV upB gridLLs gridAgg mSearch lls aggLLs probs) =
        let n = length names
            seg i = Csv.record (
                        [ Csv.toField (OutDouble (lowB !! i))
                        , Csv.toField (medV  !! i)
                        , Csv.toField (OutDouble (upB  !! i))
                        ]
                        ++ [ toFieldMaybeString (topObs !! i) | not (all isNothing topObs)]
                        ++ [ toFieldMaybeDouble (gridLLs !! i) | not (all isNothing gridLLs)]
                        ++ [ toFieldMaybeDouble (lls !! i) | not (all isNothing lls)]
                    )
        in    Csv.record [Csv.toField tsi]
           <> Csv.toRecord kernDef
           <> Csv.toRecord gridIndep
           <> maybe V.empty Csv.toRecord mSearch
           <> V.concat [ seg i | i <- [0 .. n-1] ]
           <> maybe V.empty (const $ Csv.record [toFieldMaybeDouble gridAgg]) gridAgg
           <> maybe V.empty (const $ Csv.record [toFieldMaybeDouble aggLLs]) aggLLs
           <> maybe V.empty (const $ Csv.record [toFieldMaybeDouble probs]) probs

toFieldMaybeDouble :: Maybe Double -> Bchs.ByteString
toFieldMaybeDouble Nothing  = Bchs.empty
toFieldMaybeDouble (Just x) = Csv.toField (OutDouble x)

toFieldMaybeString :: Maybe String -> Bchs.ByteString
toFieldMaybeString Nothing  = Bchs.empty
toFieldMaybeString (Just x) = Csv.toField x

-- | A data type for crossvalidation output
data CrossvalOutput = CrossvalOutput {
      _crossoutIteration        :: Int
    , _crossoutDepVars          :: DepVarName
    , _crossoutKernelDefinition :: KernelDefinition
    , _crossoutDistSum          :: Double
    , _crossoutDistMeanSquared  :: Double
    , _crossoutProbSum          :: Double
} deriving (Show, Generic)

instance NFData CrossvalOutput
instance Csv.DefaultOrdered CrossvalOutput where
    headerOrder (CrossvalOutput _ oneDepVar algo _ _ _) =
        Csv.header ["iteration", "depVar"] <> removeDepVarFromHeader oneDepVar (Csv.headerOrder algo) <> crossSummaryHeader
instance Csv.ToRecord CrossvalOutput where
    toRecord (CrossvalOutput iter oneDepVar algo sumDist meanSquaredDist sumProb) =
           Csv.toRecord [Csv.toField iter]
        <> Csv.toRecord [Csv.toField oneDepVar]
        <> Csv.toRecord algo
        <> Csv.record [Csv.toField sumDist]
        <> Csv.record [Csv.toField meanSquaredDist]
        <> Csv.record [Csv.toField $ OutDouble sumProb]

crossSummaryHeader :: Csv.Header
crossSummaryHeader = Csv.header ["sum_dep_dist_euclidean","mean_squared_dep_dist_euclidean","sum_log_likelihood"]

-- | A data type for an empirical variogram
newtype EmpiricalVariogram = EmpiricalVariogram [((Double,Double,Double), Double, Int)] -- (bin, variance, number of pairs)
    deriving Show

data EmpiricalVariogramOneVarCombination = EmpiricalVariogramOneVarCombination IndepVarName DepVarName EmpiricalVariogram
    deriving Show

data EmpiricalVariogramSingleBin = EmpiricalVariogramSingleBin {
    _evIndepVar :: IndepVarName,
    _evDepVar   :: DepVarName,
    _evBin      :: (Double,Double,Double),
    _evVariance :: Double,
    _evNrPairs  :: Int
    }
    deriving Show

instance Csv.FromNamedRecord EmpiricalVariogramSingleBin where
    parseNamedRecord m =
        EmpiricalVariogramSingleBin
            <$> filterLookup m "indepVar"
            <*> filterLookup m "depVar"
            <*> ( (,,)
                    <$> filterLookup m "bin_min"
                    <*> filterLookup m "bin_mid"
                    <*> filterLookup m "bin_max"
                )
            <*> filterLookup m "variance"
            <*> filterLookup m "nr_pairs"
instance Csv.DefaultOrdered EmpiricalVariogramSingleBin where
    headerOrder _ = Csv.header ["indepVar", "depVar", "bin_min", "bin_mid", "bin_max", "variance", "nr_pairs"]
instance Csv.ToRecord EmpiricalVariogramSingleBin where
    toRecord (EmpiricalVariogramSingleBin i d (bmin, bmid, bmax) dv npairs) =
        Csv.record [Csv.toField i, Csv.toField d, Csv.toField bmin, Csv.toField bmid, Csv.toField bmax,
                    Csv.toField dv, Csv.toField npairs]

data VariogramFit = VariogramFit
  { _vfIndepVar     :: IndepVarName
  , _vfDepVar       :: DepVarName
  , _vfKernel       :: KernelShape
  , _vfNugget       :: Double   -- nugget variance (or scaled, document!)
  , _vfSill         :: Double   -- τ²
  , _vfScaledNugget :: Double -- nug/sill
  , _vfRange        :: Double   -- lengthscale
  , _vfLoss         :: Double   -- weighted SSE
  } deriving (Show, Generic)

instance Csv.DefaultOrdered VariogramFit where
    headerOrder _ = Csv.header [ "indepVar", "depVar", "kernel", "nugget", "sill", "nugget_scaled", "range", "loss" ]
instance Csv.ToRecord VariogramFit where
    toRecord (VariogramFit iv dv kernel nug sill scalednug range loss) =
        Csv.record [ Csv.toField iv, Csv.toField dv, Csv.toField kernel, Csv.toField nug
                   , Csv.toField sill, Csv.toField scalednug, Csv.toField range, Csv.toField loss ]

-- | A data type for a dependent variable space prediction grid
newtype DepVarsPredGrid = DepVarsPredGrid [DepVarsPredPos]

-- | A data type for individual dependent variable positions
data DepVarsPredPos =
      DepVarsPredPosDirect DepVarsPos
    | DepVarsPredPosSearchObs Observation
    deriving (Show, Generic, Eq, Ord)

getObsAge :: DepVarsPredPos -> Maybe YearBCAD
getObsAge (DepVarsPredPosSearchObs (Observation _ _ (HyperPos (IndepSpatTempPos (SpatTempPos _ (TempPos obsAge))) _) _)) = Just obsAge
getObsAge _ = Nothing

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

getDepVarsPos2 :: DepVarName -> DepVarsPredPos -> Double
getDepVarsPos2 depVar (DepVarsPredPosDirect depVarsPos) = lookupUnsafe depVarsPos depVar
getDepVarsPos2 depVar (DepVarsPredPosSearchObs obs) = getDepVarsPos depVar obs

-- | A data type for preparing dependent variable positions
data DepVarsPredGridSettings =
      DirectDepVarsGridSettings [DepVarsPos]
    | SearchObsDepVarsGridSettings FilePath

-- | A data type to specify a kernel across multiple depvars and indepvars
data KernelDefinition = KernelDefinition {
        _kdefAlgorithm :: Algorithm
      , _kdefPerDepVar :: [KernelOneDepVar]
    }
    deriving (Show, Eq, Ord, Generic)

makeKernelDefinition :: Algorithm -> [KernelOneDepVar] -> KernelDefinition
makeKernelDefinition _ [] = throwL "No kernel settings provided"
makeKernelDefinition algo kerndefs =
    if allSameVars $ map _kodvLengths kerndefs
    then KernelDefinition algo $ sortBy (\k1 k2 -> compare (_kodvDepVarName k1) (_kodvDepVarName k2)) kerndefs
    else throwL "Different independent variables across dependent variables in --kerndef"

instance NFData KernelDefinition
-- the following instances differ and don't use the KernelOneDepVar instance definitions:
-- there is a conceptual difference between looking at the complete KernelDefinition, which typically exists
-- in one row, and the KernelOneDepVar values, which can form an own table for input and output
instance Csv.DefaultOrdered KernelDefinition where
    headerOrder (KernelDefinition _ l) =
        Csv.header $ ["algorithm"] ++ (map (\x -> Bchs.pack $ "kernel_" ++ x) $ concatMap oneColSet l)
        where
        oneColSet :: KernelOneDepVar -> [String]
        oneColSet (KernelOneDepVar name _ lengths (Just _)) =
            let lengthscaleCols = map (++ "_length") $ getKeys lengths
            in map (\x -> x ++ "_" ++ name) $ "shape":lengthscaleCols ++ ["nugget"]
        oneColSet (KernelOneDepVar name _ lengths Nothing) =
            let lengthscaleCols = map (++ "_length") $ getKeys lengths
            in map (\x -> x ++ "_" ++ name) $ "shape":lengthscaleCols
instance Csv.ToRecord KernelDefinition where
    toRecord (KernelDefinition algo l) =
        V.cons (Csv.toField algo) $ V.concatMap oneColSet $ V.fromList l
        where
            oneColSet :: KernelOneDepVar -> Csv.Record
            oneColSet (KernelOneDepVar _ shape lengths (Just nugget)) =
                Csv.record [Csv.toField shape] <> Csv.toRecord lengths <> Csv.record [Csv.toField nugget]
            oneColSet (KernelOneDepVar _ shape lengths Nothing) =
                Csv.record [Csv.toField shape] <> Csv.toRecord lengths
instance PseudoMap KernelDefinition KernelOneDepVar where
    toList m = zip (getKeys m) (getValues m)
    getKeys   (KernelDefinition _ l) = map _kodvDepVarName l
    getValues (KernelDefinition _ l) = l
    lookupUnsafe (KernelDefinition _ l) k =
        case find (\x -> k == _kodvDepVarName x) l of
            Just x  -> x
            Nothing -> throwL $ "Failed lookup. Missing key: " ++ k
    allSameVars xs = allEqual $ map (\(KernelDefinition _ l) -> l) xs
    filterByKey k kernDef@(KernelDefinition algo _) =
        let kernList = zip (getKeys kernDef) (getValues kernDef)
        in makeKernelDefinition algo $ map snd $ filterByKeyList k kernList

-- | A data type for a component of a kernel definition for one depvar
data KernelOneDepVar = KernelOneDepVar {
      _kodvDepVarName :: DepVarName
    , _kodvShape      :: KernelShape
    , _kodvLengths    :: KernelLengths
    , _kodvNugget     :: Maybe Double
    }
    deriving (Show, Eq, Ord, Generic)

instance NFData KernelOneDepVar
instance Csv.FromNamedRecord KernelOneDepVar where
    parseNamedRecord m = do
        depVarName <- filterLookup m "depVar"
        shape      <- filterLookup m "shape"
        lengths    <- Csv.parseNamedRecord m
        nugget     <- filterLookup m "nugget"
        pure $ KernelOneDepVar {
              _kodvDepVarName = depVarName
            , _kodvShape      = shape
            , _kodvLengths    = lengths
            , _kodvNugget     = nugget
            }
instance Csv.DefaultOrdered KernelOneDepVar where
    headerOrder (KernelOneDepVar _ _ lengths (Just _)) =
        Csv.header ["depVar"] <> Csv.header ["shape"] <> Csv.headerOrder lengths
    headerOrder (KernelOneDepVar _ _ lengths Nothing) =
        Csv.header ["depVar"] <> Csv.header ["shape"] <> Csv.headerOrder lengths <> Csv.header ["Nugget"]
instance Csv.ToRecord KernelOneDepVar where
    toRecord (KernelOneDepVar name shape lengths (Just nugget)) =
        Csv.toRecord name <> Csv.toRecord [Csv.toField shape] <> Csv.toRecord lengths <> Csv.toRecord [Csv.toField nugget]
    toRecord (KernelOneDepVar name shape lengths Nothing) =
        Csv.toRecord name <> Csv.toRecord [Csv.toField shape] <> Csv.toRecord lengths

-- type definitions for easier readability
type DepVarName   = String
type IndepVarName = String

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
    toList    (KernelLengths arbitraryDimLengths) = toList arbitraryDimLengths
    getKeys   (KernelLengths arbitraryDimLengths) = getKeys arbitraryDimLengths
    getValues (KernelLengths arbitraryDimLengths) = getValues arbitraryDimLengths
    lookupUnsafe (KernelLengths arbitraryDimLengths) = lookupUnsafe arbitraryDimLengths
    allSameVars xs = allSameVars $ map (\(KernelLengths x) -> x) xs
    filterByKey k (KernelLengths arbitraryDimLengths) = KernelLengths (filterByKey k arbitraryDimLengths)

-- | A data type to specify an interpolation algorithm
data Algorithm =
      GPR
    | KAS
    deriving (Show, Eq, Ord, Generic)

instance NFData Algorithm
instance Csv.FromField Algorithm where
    parseField x = Csv.parseField x >>= makeAlgorithm
instance Csv.ToField Algorithm where
    toField GPR = "GPR"
    toField KAS = "KAS"

makeAlgorithm :: MonadFail m => String -> m Algorithm
makeAlgorithm "GPR" = pure GPR
makeAlgorithm "KAS" = pure KAS
makeAlgorithm x     = fail $ "Algorithm " ++ show x ++ " not recognized"

-- | A data type for kernel shapes
data KernelShape =
      SquaredExponential
    | Linear
    | Exponential
    deriving (Show, Eq, Ord, Generic)

instance NFData KernelShape
instance Csv.FromField KernelShape where
    parseField x = Csv.parseField x >>= makeKernelShape
instance Csv.ToField KernelShape where
    toField SquaredExponential = "SqEx"
    toField Linear             = "Linear"
    toField Exponential        = "Ex"

makeKernelShape :: MonadFail m => String -> m KernelShape
makeKernelShape "SqEx"   = pure SquaredExponential
makeKernelShape "Linear" = pure Linear
makeKernelShape "Ex"     = pure Exponential
makeKernelShape x        = fail $ "Kernel shape " ++ show x ++ " not recognized"

-- | A data type for a per-dimension distances in independent variable space
data IndepVarsDist = IndepSpatTempDist SpatTempDist | IndepArbitraryDimDist ArbitraryDimDists
    deriving (Eq, Generic)

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
    , _obsOther :: CsvNamedRecord
} deriving (Show, Generic, Eq, Ord)

getDepVarsPos :: DepVarName -> Observation -> Double
getDepVarsPos depVar (Observation _ _ (HyperPos _ depVarsPos) _) = lookupUnsafe depVarsPos depVar

instance S.Serialise Observation
instance NFData Observation
instance Csv.FromNamedRecord Observation where
    parseNamedRecord m = do
        identifier <- filterLookup m "obsID"
        position <- Csv.parseNamedRecord m
        let alreadyConsumed = "obsID" : V.toList (Csv.headerOrder position)
            leftoverM = HM.mapMaybeWithKey (\k v -> if k `elem` alreadyConsumed then Nothing else Just v) m
        pure $ Observation {
              _obsIndex = 0
            , _obsID = identifier
            , _obsPos = position
            , _obsOther = CsvNamedRecord leftoverM
            }
instance Csv.DefaultOrdered Observation where
    headerOrder (Observation _ _ position other) =
        Csv.header ["obsID"] <> Csv.headerOrder position <> Csv.headerOrder other
instance Csv.ToRecord Observation where
    toRecord (Observation _ identifier position other) =
        Csv.record [Csv.toField identifier] <> Csv.toRecord position <> Csv.toRecord other
instance Identifiable Observation where
    getID (Observation _ identifier _ _) = identifier
    getIndex (Observation index _ _ _) = index
    setIndex x i = x {_obsIndex = i}

posFromObs :: Observation -> IndepVarsPos
posFromObs (Observation _ _ (HyperPos ivpos _) _) = ivpos

depVarPosFromObs :: Observation -> DepVarsPos
depVarPosFromObs (Observation _ _ (HyperPos _ dvpos) _) = dvpos

-- | A data type for positions in independent and dependent var space
data HyperPos = HyperPos {
      _hyposIndepVarsPos :: IndepVarsPos
    , _hyposDepVarsPos   :: DepVarsPos
} deriving (Show, Generic, Eq, Ord)

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

-- | A data type for dependent vars with some value
type DepVarsPos = ValuesPerDepVar
data ValuesPerDepVar = ValuesPerDepVar (V.Vector DepVarName) (VS.Vector Double)
    deriving (Eq, Show, Generic, Ord)

makeValuesPerDepVar :: [(DepVarName, Double)] -> ValuesPerDepVar
makeValuesPerDepVar xs =
    let sorted = sortBy (\(k1,_) (k2,_) -> compare k1 k2) xs
        namesV = V.fromList (map fst sorted)
        valsVS = VS.fromList (map snd sorted)
    in ValuesPerDepVar namesV valsVS

instance S.Serialise ValuesPerDepVar
instance NFData ValuesPerDepVar
instance Csv.FromNamedRecord ValuesPerDepVar where
    parseNamedRecord m = do
        let extractedVarsBS = HM.filterWithKey (\k _ -> Bchs.isPrefixOf "dep" k) m
            extractedVarsStringDouble = HM.mapKeys Bchs.unpack $ HM.map (read . Bchs.unpack) extractedVarsBS
        pure $ makeValuesPerDepVar $ HM.toList extractedVarsStringDouble
instance Csv.DefaultOrdered ValuesPerDepVar where
    headerOrder (ValuesPerDepVar ns _) = V.map Bchs.pack ns
instance Csv.ToRecord ValuesPerDepVar where
    toRecord (ValuesPerDepVar _ vs) = V.map (Bchs.pack . show) $ VS.convert vs
instance PseudoMap ValuesPerDepVar Double where
    toList (ValuesPerDepVar ns vs) = V.toList $ V.zip ns (VS.convert vs)
    getKeys (ValuesPerDepVar ns _) = V.toList ns
    getValues (ValuesPerDepVar _ vs) = VS.toList vs
    lookupUnsafe (ValuesPerDepVar ns vs) k =
        case V.findIndex (== k) ns of
          Just ix -> vs VS.! ix
          Nothing -> throwL ("Missing key: " ++ k)
    allSameVars xs = allEqual $ map getKeys xs
    filterByKey ks (ValuesPerDepVar ns vs) =
        let keepIx = V.findIndices (`elem` ks) ns
        in ValuesPerDepVar
             (V.backpermute ns keepIx)
             (VS.backpermute vs (VS.fromList (V.toList keepIx)))

-- | A data type for independent vars with some value
type IndepVarsThresholds = ValuesPerIndepVar
type ArbitraryDimThresholds = ValuesPerIndepVar
type ArbitraryDimPos = ValuesPerIndepVar
type ArbitraryDimDists = ValuesPerIndepVar
type ArbitraryDimLengths = ValuesPerIndepVar
data ValuesPerIndepVar = ValuesPerIndepVar (V.Vector IndepVarName) (VS.Vector Double)
    deriving (Eq, Show, Ord, Generic)

makeValuesPerIndepVar :: [(IndepVarName, Double)] -> ValuesPerIndepVar
makeValuesPerIndepVar xs =
    let sorted = sortBy (\(k1,_) (k2,_) -> compare k1 k2) xs
        namesV = V.fromList (map fst sorted)
        valsVS = VS.fromList (map snd sorted)
    in ValuesPerIndepVar namesV valsVS

instance S.Serialise ValuesPerIndepVar
instance NFData ValuesPerIndepVar
instance Csv.FromNamedRecord ValuesPerIndepVar where
    parseNamedRecord m = do
        -- pretty hacky to give space and time a special role here
        let extractedVarsBS = HM.filterWithKey (\k _ -> Bchs.isPrefixOf "indep" k || k == "space" || k == "time") m
            extractedVarsStringDouble = HM.mapKeys Bchs.unpack $ HM.map (read . Bchs.unpack) extractedVarsBS
        pure $ makeValuesPerIndepVar $ HM.toList extractedVarsStringDouble
instance Csv.DefaultOrdered ValuesPerIndepVar where
    headerOrder (ValuesPerIndepVar ns _) = V.map Bchs.pack ns
instance Csv.ToRecord ValuesPerIndepVar where
    toRecord (ValuesPerIndepVar _ vs) = V.map (Bchs.pack . show) $ VS.convert vs
instance PseudoMap ValuesPerIndepVar Double where
    toList (ValuesPerIndepVar ns vs) = V.toList $ V.zip ns (VS.convert vs)
    getKeys (ValuesPerIndepVar ns _) = V.toList ns
    getValues (ValuesPerIndepVar _ vs) = VS.toList vs
    lookupUnsafe (ValuesPerIndepVar ns vs) k =
        case V.findIndex (== k) ns of
          Just ix -> vs VS.! ix
          Nothing -> throwL ("Missing key: " ++ k)
    allSameVars xs = allEqual $ map getKeys xs
    filterByKey ks (ValuesPerIndepVar ns vs) =
        let keepIx = V.findIndices (`elem` ks) ns
        in ValuesPerIndepVar
             (V.backpermute ns keepIx)
             (VS.backpermute vs (VS.fromList (V.toList keepIx)))

anyPosFromDepVarsPos :: DepVarsPos -> VS.Vector Double
anyPosFromDepVarsPos (ValuesPerDepVar _ v) = v

-- A data type for positions independent variable space, so here either a spatiotemporal
-- or an arbitrary space
data IndepVarsPos = IndepSpatTempPos SpatTempPos | IndepArbitraryDimPos ArbitraryDimPos
    deriving (Eq, Show, Generic, Ord)

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

spatPosFromIndepVarsPos :: IndepVarsPos -> SpatPos
spatPosFromIndepVarsPos (IndepSpatTempPos (SpatTempPos s _)) = s
spatPosFromIndepVarsPos pos = throwL $ "Expected spatio-temporal position, but got " ++ show pos

tempPosFromIndepVarsPos :: IndepVarsPos -> TempPos
tempPosFromIndepVarsPos (IndepSpatTempPos (SpatTempPos _ t)) = t
tempPosFromIndepVarsPos pos = throwL $ "Expected spatio-temporal position, but got " ++ show pos

anyPosFromIndepVarsPos :: IndepVarsPos -> VS.Vector Double
anyPosFromIndepVarsPos (IndepArbitraryDimPos (ValuesPerIndepVar _ v)) = v
anyPosFromIndepVarsPos pos = throwL $ "Expected arbitrary dimension position, but got " ++ show pos

isSpatioTemporal :: V.Vector IndepVarsPos -> Bool
isSpatioTemporal v = case v V.!? 0 of
    Just (IndepSpatTempPos _) -> True
    _                         -> False

-- | A data type for distances in space and time
data SpatTempDist = SpatTempDist {
      _spatDist :: Double
    , _tempDist :: Double
    }
    deriving (Eq, Generic)

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
    }
    deriving (Eq, Show, Generic, Ord)

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

-- | A data type for one age sample (used for reading from .tsv)
data TempSample = TempSample {
      _tempSampObsID :: String
    , _tempSampAge   :: YearBCAD
    }
    deriving (Show, Generic)

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
    deriving (Eq, Show, Generic, Ord)

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
    deriving (Eq, Show, Generic, Ord)

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
    deriving (Eq, Show, Generic, Ord)

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
    deriving (Eq, Show, Generic, Ord)

instance S.Serialise LongLatPos
instance NFData LongLatPos
instance Csv.FromNamedRecord LongLatPos where
    parseNamedRecord m =
        LongLatPos
        <$> pure 0
        <*> filterLookupOptional m "spatID"
        <*> filterLookup m "longitude"
        <*> filterLookup m "latitude"
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
    deriving (Eq, Show, Generic, Ord)

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
    deriving (Eq, Show, Generic, Ord)

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

{-# LANGUAGE BangPatterns #-}

module LocEst.CLI.Vario where

import           LocEst.Distance
import           LocEst.Parsers
import           LocEst.Types
import           LocEst.TypesFlat
import           LocEst.Utils

import           Conduit                      ((.|))
import qualified Control.Monad                as OP
import qualified Data.Conduit                 as Con
import qualified Data.Conduit.Combinators     as ConC
import qualified Data.Conduit.List            as ConL
import           Data.Function                (on)
import           Data.List                    (singleton, sort)
import qualified Data.Vector                  as V
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Unboxed          as VU
import           LocEst.CLI.Cross             (splitIdx)
import           System.IO                    (hPutStrLn, stderr)
import qualified System.Random                as R

data VarioOptions = VarioOptions {
      _voInObservationFile        :: FilePath
    , _voInObsObsDistFile         :: Maybe FilePath
    , _voAcrossSettings           :: AcrossSettings
    , _voSpaceTimeScaling         :: (Double,Double)
    , _voIndepVarsThresholds      :: IndepVarsThresholds
    , _voIndepVarsCrossThresholds :: IndepVarsThresholds
    , _voSubsamplingIters         :: Int -- 0 = no subsampling
    , _voSubsamplingFrac          :: Double
    , _voSubsamplingMaybeSeed     :: Maybe Int
    , _voOutFile                  :: Maybe FilePath
    , _voBinMode                  :: BinModeSettings
}

data AcrossSettings =
      AcrossNone
    | AcrossIndepVars
    | AcrossDepVars
    | AcrossBoth
    | AcrossComb

instance Show AcrossSettings where
    show AcrossNone = "No merging of distances"
    show AcrossIndepVars = "Merge independent variable distances"
    show AcrossDepVars = "Merge independent variable distances"
    show AcrossBoth = "Merge both independent and dependent variable distances"
    show AcrossComb = "Iterate through all modes"

data BinModeSettings =
      BinByNrBins Int
    | BinForNugget ArbitraryDimPos
    deriving (Show)

runVario :: VarioOptions -> Double -> IO ()
runVario
    (VarioOptions inObsFile maybeObsObsDistFile acrossSetting (spaceScaling,timeScaling)
    indepVarsThresholds indepVarsCrossThresholds subsamplingIters subsamplingFrac subsamplingSeed outFile binModeSettings)
    spatDistUnitScaling = do
    -- read observations
    !obs <- readObservations inObsFile
    let nObs = V.length obs
    -- read distances
    !obsObsDistances <- traverse (readSelfDistMulti nObs) maybeObsObsDistFile
    -- prepare subsampling plan
    subsamplingPlan <- case subsamplingIters of
        0 -> pure [(0, Nothing)] -- no subsampling
        iters -> do
            baseSeed <- case subsamplingSeed of
                Just s  -> pure s
                Nothing -> R.randomRIO (0, maxBound :: Int)
            hPutStrLn stderr $ "Seed for subsampling: " ++ show baseSeed
            let nRemove = round (subsamplingFrac * fromIntegral nObs)
            return [(iter, Just (subsamplingRemoveIdx (baseSeed + iter) nRemove nObs)) | iter <- [1 .. iters]]
    -- configure across-settings
    hPutStrLn stderr $ "Distance merging mode: " ++ show acrossSetting
    let acrossModes = case acrossSetting of
            AcrossNone      -> [(False, False)]
            AcrossIndepVars -> [(True,  False)]
            AcrossDepVars   -> [(False, True )]
            AcrossBoth      -> [(True,  True )]
            AcrossComb      -> [(False, False), (True, False), (False, True), (True, True)]
    -- compute variograms
    -- loop over variable merging "across" settings
    empiricalVariograms <- forM acrossModes $ \(acrossIndepVars, acrossDepVars) -> do
        hPutStrLn stderr $ "Merging variables: "
            ++ (if acrossIndepVars then "[x]" else "[ ]") ++ " Independent, "
            ++ (if acrossDepVars   then "[x]" else "[ ]") ++ " Dependent"
        -- pairwise distances
        hPutStrLn stderr "Reading or calculating pairwise distances..."
        -- distances independent variables
        let indepVars = case posFromObs $ V.head obs of
                IndepSpatTempPos _     -> ["space", "time"]
                IndepArbitraryDimPos x -> getKeys x
        !rawIndepDists <- case obsObsDistances of
            Nothing -> calcObsObsDistances spatDistUnitScaling obs indepVars
            Just (SelfDistMatrixPerIndepVar ms) -> do
                SelfDistMatrixPerIndepVar <$>
                    forM indepVars (\name -> case lookup name ms of
                        Just m  -> pure (name, m)
                        Nothing -> calcSelfDistOneDim spatDistUnitScaling
                                   (\(Observation _ _ (HyperPos pos _) _) -> pos) obs name)
        !distsPerIndepVar <- if acrossIndepVars
                             then mergeDistsIndepVar (spaceScaling, timeScaling) rawIndepDists
                             else pure rawIndepDists
        -- distances dependent variables
        let depVars = getKeys $ depVarPosFromObs $ V.head obs
        !distsPerDepVar <- if acrossDepVars
                           then do
                                 allDists <- calcObsObsDistDepVar obs depVars
                                 mergeDistsDepVar allDists
                           else calcObsObsDistDepVar obs depVars
        hPutStrLn stderr "Calculating empirical variograms..."
        -- loop over subsampling iterations
        forM subsamplingPlan $ \(subsamplingIter, maybeRemoveIdx) -> do
            let !distsPerIndepVar' = maybe rawIndepDists (\rm -> removeObservationsMulti nObs rm distsPerIndepVar) maybeRemoveIdx
                !distsPerDepVar' = maybe distsPerDepVar (\rm -> removeObservationsMulti nObs rm distsPerDepVar) maybeRemoveIdx
            OP.when (subsamplingIters > 0) $ hPutStrLn stderr $ "Subsampling iteration: " ++ show subsamplingIter
            -- loop over all permutations of indepVars and depVars to calculate empirical variograms
            fmap concat $
                -- loop over indepVars
                forM (toList distsPerIndepVar') $ \(indepVarName, SelfDistMatrix indepDists) -> do
                    -- indexing (must be done before any filtering)
                    let indepDistsIndexed = VU.indexed $ VS.convert indepDists
                        indepDistsIndexedModified =
                            if not acrossIndepVars
                            then do
                                -- indepVar filtering
                                let indepDistsFiltered =
                                        case filter (\(name,_) -> name == indepVarName) $ toList indepVarsThresholds of
                                            [(_,relevantThreshold)] -> VU.filter ((<= relevantThreshold) . snd) indepDistsIndexed
                                            _                       -> indepDistsIndexed
                                -- indepVar cross-filtering
                                    indepDistsCrossFiltered =
                                        let relevantThresholds = filter (\(name,_) -> name /= indepVarName) $ toList indepVarsCrossThresholds
                                            belowThresholdPerIndepVar = map (VU.convert . isBelowIndepVarsThreshold distsPerIndepVar') relevantThresholds
                                            belowAllThresholds = foldl' (VU.zipWith (&&)) (VU.replicate (VS.length indepDists) True) belowThresholdPerIndepVar
                                        in VU.map snd $ VU.filter fst $ VU.zip belowAllThresholds indepDistsFiltered
                                 in indepDistsCrossFiltered
                            else indepDistsIndexed
                    -- sort indep distance vector for easy binning
                    sortedIndepDists <- sortWithIndices indepDistsIndexedModified -- very time-consuming!
                    -- get start index and stop index for each bin in the sorted indep vector
                    let startStopPerBin = case binModeSettings of
                            BinByNrBins nrBins      -> binIndepVarByNrBins sortedIndepDists nrBins
                            BinForNugget thresholds ->
                                if acrossIndepVars && (sort (getKeys thresholds) == ["space", "time"])
                                then
                                    let spaceThreshold  = lookupUnsafe thresholds "space"
                                        timeThreshold   = lookupUnsafe thresholds "time"
                                        mergedThreshold = sqrt (((spaceThreshold / spaceScaling) ** 2) + (timeThreshold / timeScaling) ** 2)
                                    in binIndepVarForNugget sortedIndepDists (makeValuesPerIndepVar [("acrossIndep", mergedThreshold)]) indepVarName
                                else binIndepVarForNugget sortedIndepDists thresholds indepVarName
                    -- add infinite bin to compute total variance in filtered (!) distances
                    -- let allBins = startStopPerBin ++ [((0, inf, inf), 0, VU.length sortedIndepDists - 1)]
                    -- loop over depVars
                    forM (toList distsPerDepVar') $ \(depVarName, SelfDistMatrix depDists) -> do
                        -- loop over bins
                        variancesPerBin <- Con.runConduitRes $
                                ConC.yieldMany startStopPerBin
                                .| ConL.map (perBin sortedIndepDists $ VU.convert depDists)
                                .| ConC.sinkList
                        -- add infinite bin with total variance across all (!) distances
                        let totalVarianceForDepVar = calcHalfMeanSquared $ VU.convert depDists
                            withInfiniteBin = variancesPerBin ++ [((0, inf, inf), totalVarianceForDepVar, VS.length indepDists)]
                        hPutStrLn stderr (indepVarName ++ " -> " ++ depVarName)
                        return $ EmpiricalVariogramOneVarCombination subsamplingIter indepVarName depVarName (EmpiricalVariogram withInfiniteBin)
    -- write variograms to the file system
    hPutStrLn stderr "Writing result table..."
    writeVariograms (concat $ concat empiricalVariograms) outFile
    hPutStrLn stderr "Done"

isBelowIndepVarsThreshold :: SelfDistMatrixPerIndepVar -> (IndepVarName, Double) -> VS.Vector Bool
isBelowIndepVarsThreshold distsPerIndepVar (indepVarName, threshold) =
    let (SelfDistMatrix dists) = lookupUnsafe distsPerIndepVar indepVarName
    in VS.map (<=threshold) dists

-- write variograms to the file system
writeVariograms :: [EmpiricalVariogramOneVarCombination] -> Maybe FilePath -> IO ()
writeVariograms vars path = Con.runConduitRes $ ConC.yieldMany (concatMap varToLong vars) .| sinkNamedCSV path
    where
        varToLong :: EmpiricalVariogramOneVarCombination -> [EmpiricalVariogramSingleBin]
        varToLong (EmpiricalVariogramOneVarCombination subsamplingIter i d (EmpiricalVariogram xs)) =
            map (\(iv, dv, nrPairs) -> EmpiricalVariogramSingleBin subsamplingIter i d iv dv nrPairs) xs

perBin :: VU.Vector (Int, Double) -> VU.Vector Double -> ((Double,Double,Double), Int, Int) -> ((Double,Double,Double), Double, Int)
perBin sortedIndepDists depDists (minMidMax, startSorted, stopSorted) =
    let indicesForThisBin = getIndicesForBin sortedIndepDists startSorted stopSorted
        depDistsPerBin = VU.map (depDists VU.!) indicesForThisBin
        nrPairs = VU.length depDistsPerBin
        -- calculate variance per bin
        variance = calcHalfMeanSquared depDistsPerBin
    in (minMidMax, variance, nrPairs)

-- perform binning of an indepVar
binIndepVarForNugget :: VU.Vector (Int, Double) -> ArbitraryDimPos -> IndepVarName -> [((Double, Double, Double), Int, Int)]
binIndepVarForNugget sortedVec thresholds indepVarName =
    let threshold = lookupUnsafe thresholds indepVarName
        stop = case VU.findIndexR (\(_,x) -> x <= threshold) sortedVec of
            Nothing -> VU.length sortedVec - 1
            Just i  -> i
    in singleton (binMinMidMax sortedVec 0 stop, 0, stop)

binIndepVarByNrBins :: VU.Vector (Int, Double) -> Int -> [((Double, Double, Double), Int, Int)]
binIndepVarByNrBins sortedVec nrBins =
    let len = VU.length sortedVec
        stepWidth = len `div` nrBins -- in nr of distances
        starts = [0,stepWidth..(len - stepWidth)]
        stops = map (\x -> x-1) [stepWidth,2*stepWidth..len]
    in zipWith (\start stop -> (binMinMidMax sortedVec start stop, start, stop)) starts stops

binMinMidMax :: VU.Vector (Int, Double) -> Int -> Int -> (Double, Double, Double)
binMinMidMax sortedVec start stop =
    let (_,lo) = sortedVec VU.! start
        (_,hi) = sortedVec VU.! stop
    in (lo,(lo+hi)/2,hi)

-- mean squared distance within one bin
-- matheron estimator
calcHalfMeanSquared :: VU.Vector Double -> Double
calcHalfMeanSquared dists =
    let n = fromIntegral $ VU.length dists
    in (1 / (2*n)) * VU.foldl' (\acc d -> acc + (d ** 2)) 0 dists

sortWithIndices :: VU.Vector (Int, Double) -> IO (VU.Vector (Int, Double))
sortWithIndices v = do
    mv <- VU.thaw v    -- Create a mutable copy
    VA.sortBy (compare `on` snd) mv -- Sort it in-place
    VU.unsafeFreeze mv -- Convert back to a pure vector
getIndicesForBin :: VU.Vector (Int, Double) -> Int -> Int -> VU.Vector Int
getIndicesForBin sortedVec i1 i2 =
    --let !_ = unsafePerformIO $ putStrLn (show i1 ++ " " ++ show (i2 - i1))
    VU.map fst $ VU.slice i1 (i2 - i1 + 1) sortedVec

-- subsampling
subsamplingRemoveIdx :: Int -> Int -> Int -> VS.Vector Int
subsamplingRemoveIdx seed nRemove nObs = fst $ splitIdx seed nRemove nObs

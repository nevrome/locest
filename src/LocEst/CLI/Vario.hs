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
import           Data.List                    (singleton, sort)
import qualified Data.Vector                  as V
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Storable         as VS
import           System.IO                    (hPutStrLn, stderr)
import qualified System.Random                as R
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word (Word32)

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
    show AcrossDepVars = "Merge dependent variable distances"
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
            return [(iter, Just (fst $ splitIdx (baseSeed + iter) nRemove nObs)) | iter <- [1 .. iters]]
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
            let !distsPerIndepVar' = maybe distsPerIndepVar (\rm -> removeObservationsMulti nObs rm distsPerIndepVar) maybeRemoveIdx
                !distsPerDepVar' = maybe distsPerDepVar (\rm -> removeObservationsMulti nObs rm distsPerDepVar) maybeRemoveIdx
            OP.when (subsamplingIters > 0) $ hPutStrLn stderr $ "Subsampling iteration: " ++ show subsamplingIter
            -- loop over all permutations of indepVars and depVars to calculate empirical variograms
            fmap concat $
                -- loop over indepVars
                forM (toList distsPerIndepVar') $ \(indepVarName, SelfDistMatrix indepDists) -> do
                    let mainThreshold =
                            case [thr | (name, thr) <- toList indepVarsThresholds, name == indepVarName] of
                                 [thr] -> Just thr
                                 _     -> Nothing
                        crossThresholds =
                            [ let SelfDistMatrix v = lookupUnsafe distsPerIndepVar' name in (v, threshold)
                            | (name, threshold) <- toList indepVarsCrossThresholds, name /= indepVarName ]
                    sortedIdxs <- buildAndSortCandidateIndices indepDists mainThreshold crossThresholds
                    -- get start index and stop index for each bin in the sorted indep vector
                    let startStopPerBin =
                            case binModeSettings of
                                BinByNrBins nrBins -> binIndepVarByNrBinsIdx indepDists sortedIdxs nrBins
                                BinForNugget thresholds ->
                                    let threshold = if acrossIndepVars && sort (getKeys thresholds) == ["space", "time"]
                                                    then let spaceThreshold  = lookupUnsafe thresholds "space"
                                                             timeThreshold   = lookupUnsafe thresholds "time"
                                                         in sqrt (((spaceThreshold / spaceScaling) ** 2) + ((timeThreshold / timeScaling) ** 2))
                                            else lookupUnsafe thresholds indepVarName
                                    in binIndepVarForNuggetIdx indepDists sortedIdxs threshold
                    -- loop over depVars
                    forM (toList distsPerDepVar') $ \(depVarName, SelfDistMatrix depDists) -> do
                        -- loop over bins
                        variancesPerBin <- Con.runConduitRes $
                                ConC.yieldMany startStopPerBin
                                .| ConL.map (perBinIdx sortedIdxs depDists)
                                .| ConC.sinkList
                        -- add infinite bin with total variance across all (!) distances
                        let totalVarianceForDepVar = calcHalfMeanSquared depDists
                            withInfiniteBin = variancesPerBin ++ [((0, inf, inf), totalVarianceForDepVar, VS.length depDists)]
                        hPutStrLn stderr (indepVarName ++ " -> " ++ depVarName)
                        return $ EmpiricalVariogramOneVarCombination subsamplingIter indepVarName depVarName (EmpiricalVariogram withInfiniteBin)
    -- write variograms to the file system
    hPutStrLn stderr "Writing result table..."
    writeVariograms (concat $ concat empiricalVariograms) outFile
    hPutStrLn stderr "Done"

splitIdx :: Int -> Int -> Int -> (VS.Vector Int, VS.Vector Int)
splitIdx seed nTest n =
    let rng = R.mkStdGen seed
        idxs = V.fromList [0..n-1]
        (shuffled,_) = shuffle idxs rng
    in VS.splitAt nTest (VS.convert shuffled)

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

buildAndSortCandidateIndices :: VS.Vector Double -> Maybe Double -> [(VS.Vector Double, Double)] -> IO (VS.Vector Word32)
buildAndSortCandidateIndices indepDists maybeMainThreshold crossThresholds = do
    let !n = VS.length indepDists
    let countLoop !k !acc
            | k == n = acc
            | passes k = countLoop (k + 1) (acc + 1)
            | otherwise = countLoop (k + 1) acc
        !outLen = countLoop 0 0
    mv <- VSM.unsafeNew outLen
    let fillLoop !k !out
            | k == n = pure ()
            | passes k = do
                VSM.unsafeWrite mv out (fromIntegral k)
                fillLoop (k + 1) (out + 1)
            | otherwise = fillLoop (k + 1) out
    fillLoop 0 0
    VA.sortBy (\a b ->
            compare
                (VS.unsafeIndex indepDists (fromIntegral a))
                (VS.unsafeIndex indepDists (fromIntegral b))) mv
    VS.unsafeFreeze mv
    where
    passes !k = mainOk && crossOk
        where
            !d = VS.unsafeIndex indepDists k
            !mainOk = case maybeMainThreshold of
                            Nothing  -> True
                            Just thr -> d <= thr
            !crossOk = all (\(v, thr) -> VS.unsafeIndex v k <= thr) crossThresholds

binIndepVarByNrBinsIdx :: VS.Vector Double -> VS.Vector Word32 -> Int -> [((Double, Double, Double), Int, Int)]
binIndepVarByNrBinsIdx indepDists sortedIdxs nrBins =
    let len = VS.length sortedIdxs
        stepWidth = len `div` nrBins
        starts = [0, stepWidth .. (len - stepWidth)]
        stops = map (\x -> x - 1) [stepWidth, 2 * stepWidth .. len]
    in zipWith (\start stop -> (binMinMidMaxIdx indepDists sortedIdxs start stop, start, stop)) starts stops

binIndepVarForNuggetIdx :: VS.Vector Double -> VS.Vector Word32 -> Double -> [((Double, Double, Double), Int, Int)]
binIndepVarForNuggetIdx indepDists sortedIdxs threshold =
    let stop = case VS.findIndexR (\ix -> VS.unsafeIndex indepDists (fromIntegral ix) <= threshold) sortedIdxs of
                Nothing -> VS.length sortedIdxs - 1
                Just i  -> i
    in singleton (binMinMidMaxIdx indepDists sortedIdxs 0 stop, 0, stop)

binMinMidMaxIdx :: VS.Vector Double -> VS.Vector Word32 -> Int -> Int -> (Double, Double, Double)
binMinMidMaxIdx indepDists sortedIdxs start stop =
    let !loIx = fromIntegral $ VS.unsafeIndex sortedIdxs start
        !hiIx = fromIntegral $ VS.unsafeIndex sortedIdxs stop
        !lo   = VS.unsafeIndex indepDists loIx
        !hi   = VS.unsafeIndex indepDists hiIx
    in (lo, (lo + hi) / 2, hi)

perBinIdx :: VS.Vector Word32 -> VS.Vector Double -> ((Double, Double, Double), Int, Int) -> ((Double, Double, Double), Double, Int)
perBinIdx sortedIdxs depDists (minMidMax, startSorted, stopSorted) =
    let go !k !acc !count
            | k > stopSorted =
                let !variance =
                        if count == 0
                        then 0 / 0
                        else acc / (2 * fromIntegral count)
                in (minMidMax, variance, count)
            | otherwise =
                let !pairIx = fromIntegral $ VS.unsafeIndex sortedIdxs k
                    !d      = VS.unsafeIndex depDists pairIx
                in go (k + 1) (acc + d * d) (count + 1)
    in go startSorted 0 0

-- mean squared distance within one bin
-- matheron estimator
calcHalfMeanSquared :: VS.Vector Double -> Double
calcHalfMeanSquared dists =
    let !n = fromIntegral $ VS.length dists
        go !i !acc
            | i == VS.length dists = acc
            | otherwise =
                let !d = VS.unsafeIndex dists i
                in go (i + 1) (acc + d * d)
    in (1 / (2 * n)) * go 0 0

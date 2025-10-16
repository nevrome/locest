{-# LANGUAGE BangPatterns #-}

module LocEst.CLI.Vario where

import           LocEst.Utils
import           LocEst.Distance
import           LocEst.Parsers
import           LocEst.Types
import           LocEst.TypesFlat

import           Conduit                       ((.|))
import           Control.Monad                 (replicateM, zipWithM_)
import qualified Data.Conduit                  as Con
import qualified Data.Conduit.Algorithms.Async as ConAA
import qualified Data.Conduit.Combinators      as ConC
import           Data.Function                 (on)
import           Data.List                     (foldl', singleton, sort)
import qualified Data.Vector                   as V
import qualified Data.Vector.Algorithms.Intro  as VA
import qualified Data.Vector.Storable          as VS
import qualified Data.Vector.Storable.Mutable  as VSM
import qualified Data.Vector.Unboxed           as VU
import           System.FilePath               (takeExtension)
import           System.IO                     (hPutStrLn, stderr)

data VarioOptions = VarioOptions {
      _voInObservationFile   :: FilePath
    , _voSpatDistSetting     :: Maybe SpatDistSettings
    , _voAcrossSettings      :: AcrossSettings
    , _voSpaceTimeScaling    :: (Double,Double)
    , _voIndepVarsThresholds :: IndepVarsThresholds
    , _voOutFile             :: Maybe FilePath
    , _voBinMode             :: BinModeSettings
}

data SpatDistSettings = SpatDistSettings {
      _sdfInSpatDistFile :: FilePath
    , _sdfNoOrderCheck   :: Bool
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

runVario :: VarioOptions -> Int -> Double -> IO ()
runVario
    (VarioOptions inObsFile maybeSpatDist acrossSetting (spaceScaling,timeScaling) indepVarsThresholds outFile binModeSettings)
    numThreads spatDistUnitScaling = do
    -- read observations
    !obs <- readObservations inObsFile
    -- configure across-settings
    hPutStrLn stderr $ "Distance merging mode: " ++ show acrossSetting
    let acrossModes = case acrossSetting of
            AcrossNone      -> [(False, False)]
            AcrossIndepVars -> [(True,  False)]
            AcrossDepVars   -> [(False, True )]
            AcrossBoth      -> [(True,  True )]
            AcrossComb      -> [(False, False), (True, False), (False, True), (True, True)]
    -- compute variograms
    empiricalVariograms <- forM acrossModes $ \(acrossIndepVars, acrossDepVars) -> do
        hPutStrLn stderr $ "Merging variables: "
            ++ (if acrossIndepVars then "[x]" else "[ ]") ++ " Independent, "
            ++ (if acrossDepVars   then "[x]" else "[ ]") ++ " Dependent"
        -- calculate pairwise distances
        hPutStrLn stderr "Calculating pairwise distances for independent variables"
        let indepVars = case posFromObs $ V.head obs of
                IndepSpatTempPos _ -> ["space", "time"]
                IndepArbitraryDimPos x -> getKeys x
        -- only computes half of the pairwise distances
        !distsPerIndepVar <- if acrossIndepVars
                             then do
                                 allDists <- calcObsObsDistances spatDistUnitScaling obs indepVars
                                 mergeDistsIndepVar (spaceScaling,timeScaling) allDists
                             else calcObsObsDistances spatDistUnitScaling obs indepVars
        hPutStrLn stderr "Calculating pairwise distances for dependent variables"
        -- only computes half of the pairwise distances
        let depVars = getKeys $ depVarPosFromObs $ V.head obs
        SUDistMatrixPerIndepVar !distsPerDepVar <-
                             if acrossDepVars
                             then do
                                 allDists <- calcObsObsDistDepVar obs depVars
                                 mergeDistsDepVar allDists
                             else calcObsObsDistDepVar obs depVars
        -- iterate over all permutations of indepVars and depVars to calculate empirical variograms
        hPutStrLn stderr "Calculating empirical variograms"
        fmap concat $
            -- loop over indepVars
            forM (toList distsPerIndepVar) $ \(indepVarName, SUDistMatrix indepDists) -> do
                hPutStrLn stderr ("Working on " ++ indepVarName)
                -- indexing (must be done before any filtering)
                let indepDistsIndexed = VU.indexed $ VS.convert indepDists
                -- indepVar cross-filtering - only when acrossIndepVars is inactive
                    indepDistsFiltered =
                        if acrossIndepVars
                        then indepDistsIndexed
                        else
                            let relevantThresholds = filter (\(name,_) -> name /= indepVarName) $ toList indepVarsThresholds
                                belowThresholdPerIndepVar = map (VU.convert . isBelowIndepVarsThreshold distsPerIndepVar) relevantThresholds
                                belowAllThresholds = foldl' (VU.zipWith (&&)) (VU.replicate (VS.length indepDists) True) belowThresholdPerIndepVar
                            in VU.map snd $ VU.filter fst $ VU.zip belowAllThresholds indepDistsIndexed
                -- sort indep distance vector for easy binning
                sortedIndepDists <- sortWithIndices indepDistsFiltered -- very time-consuming!
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
                -- loop over depVars
                forM distsPerDepVar $ \(depVarName, SUDistMatrix depDists) -> do
                    -- loop over bins
                    variancesPerBin <- Con.runConduitRes $
                            ConC.yieldMany startStopPerBin
                            .| ConAA.asyncMapC numThreads (perBin sortedIndepDists $ VU.convert depDists)
                            .| ConC.sinkList
                    hPutStrLn stderr ("-> " ++ depVarName)
                    return $ EmpiricalVariogramOneVarCombination indepVarName depVarName (EmpiricalVariogram variancesPerBin)
    -- write variograms to the file system
    writeVariograms (concat empiricalVariograms) outFile
    hPutStrLn stderr "Done"

isBelowIndepVarsThreshold :: SUDistMatrixPerIndepVar -> (IndepVarName, Double) -> VS.Vector Bool
isBelowIndepVarsThreshold distsPerIndepVar (indepVarName, threshold) =
    let (SUDistMatrix dists) = lookupUnsafe distsPerIndepVar indepVarName
    in VS.map (<=threshold) dists

-- write variograms to the file system
writeVariograms :: [EmpiricalVariogramOneVarCombination] -> Maybe FilePath -> IO ()
writeVariograms vars path = Con.runConduitRes $ ConC.yieldMany (concatMap varToLong vars) .| sinkNamedCSV path
    where
        varToLong :: EmpiricalVariogramOneVarCombination -> [EmpiricalVariogramSingleBin]
        varToLong (EmpiricalVariogramOneVarCombination i d (EmpiricalVariogram xs)) =
            map (\(iv, dv) -> EmpiricalVariogramSingleBin i d iv dv) xs

perBin :: VU.Vector (Int, Double) -> VU.Vector Double -> ((Double,Double,Double), Int, Int) -> ((Double,Double,Double), Double)
perBin sortedIndepDists depDists (minMidMax, startSorted, stopSorted) =
    let indicesForThisBin = getIndicesForBin sortedIndepDists startSorted stopSorted
        depDistsPerBin = VU.map (depDists VU.!) indicesForThisBin
        -- calculate variance per bin
        variance = calcHalfMeanSquared depDistsPerBin
    in (minMidMax, variance)

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
    VU.map fst $ VU.slice i1 (i2 - i1) sortedVec

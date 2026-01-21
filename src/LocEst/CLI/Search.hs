{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module LocEst.CLI.Search where

import           LocEst.CoreAlgorithms
import           LocEst.Distance
import           LocEst.Parsers
import           LocEst.Types
import           LocEst.TypesFlat
import           LocEst.Utils

import           Conduit                  (liftIO)
import           Data.Conduit             ((.|))
import qualified Data.Conduit             as Con
import qualified Data.Conduit.Combinators as ConC
import qualified Data.Conduit.List        as ConL
import           Data.Foldable            (foldl')
import           Data.List                (intercalate)
import qualified Data.Map.Strict          as Map
import           Data.Maybe               (isJust, mapMaybe)
import qualified Data.Vector              as V
import           System.IO                (hPutStrLn, stderr)

data SearchOptions = SearchOptions
    { _searchInObservationFile   :: FilePath
    , _searchInTempSampFile      :: Maybe FilePath
    , _searchInIndepPredGridFile :: FilePath
    , _searchInTempGrid          :: Maybe [AbsRelTempPos]
    , _searchInDepSearchGrid     :: Maybe DepVarsPredGridSettings
    , _searchAlgorithm           :: KernelDefinition
    , _searchInObsGridDistFile   :: Maybe FilePath
    , _searchInObsObsDistFile    :: Maybe FilePath
    , _searchInGridGridDistFile  :: Maybe FilePath
    , _searchOutFile             :: Maybe FilePath
    }

runSearch :: SearchOptions -> Double -> IO ()
runSearch (SearchOptions
    inObsFile maybeTempSampFile inIndepVarsPredGridFile maybeTempGrid
    inMaybeDepSearchGrid kernelDefinition
    maybeObsGridDistFile maybeObsObsDistFile maybeGridGridDistFile
    outFile
    ) spatDistUnitScaling = do
    -- algorithm settings
    let algorithm = _kdefAlgorithm kernelDefinition
        depVars   = getKeys kernelDefinition
        indepVars = getKeys $ _kodvLengths $ head $ _kdefPerDepVar kernelDefinition
        kernels   = getValues kernelDefinition
    hPutStrLn stderr $ "Algorithm: " ++ show algorithm
    hPutStrLn stderr $ "Dependent variables: " ++ intercalate ", " depVars
    hPutStrLn stderr $ "Independent variables: " ++ intercalate ", " indepVars
    -- read observations
    !obs <- filterVarsInObs depVars indepVars <$> readObservations inObsFile
    let nObs = V.length obs
    hPutStrLn stderr $ "Number of observations: " ++ show nObs
    -- read temporal resampling iterations
    !tempSamp <- traverse (readTempSamp obs) maybeTempSampFile
    -- read indepVar prediction grid positions
    !indepPredGrid <- V.map (filterVarsInIndepVarsPos indepVars) <$> readIndepVarsPos inIndepVarsPredGridFile
    let nGrid = V.length indepPredGrid
    hPutStrLn stderr $ "Number of grid positions: " ++ show nGrid
    -- read depVar search grid
    !depSearchGrid <- traverse (readDepVarsPredGrid depVars indepVars) inMaybeDepSearchGrid
    -- read distances
    !obsGridDistances  <- traverse (readAUDistMulti nObs nGrid) maybeObsGridDistFile
    !obsObsDistances   <- traverse (readSUDistMulti nObs) maybeObsObsDistFile
    !gridGridDistances <- traverse (readSUDistMulti nGrid) maybeGridGridDistFile
    -- permutations
    hPutStrLn stderr "Preparing permutations"
    let permutations = createPermutations obs tempSamp indepPredGrid depSearchGrid maybeTempGrid
        -- the number of permutations depends on
        -- the temporal grid,
        -- the temporal resampling,
        -- the search grid
        nrOutputRows = case maybeTempGrid of
            Nothing -> length indepPredGrid
            Just tempGrid -> case depSearchGrid of
                Nothing -> length indepPredGrid * length tempGrid
                Just searchGrid -> length indepPredGrid * length tempGrid * length searchGrid
    -- run interpolation and search
    hPutStrLn stderr "Running interpolation"
    Con.runConduitRes $
           ConC.yieldMany permutations
        .| ConL.concatMapM (liftIO . search algorithm indepVars obsGridDistances obsObsDistances gridGridDistances spatDistUnitScaling depVars kernels)
        .| progress 1000 (Just nrOutputRows)
        .| sinkNamedCSV outFile
    putStrLn "Done"

search :: Algorithm
     -> [IndepVarName]
     -> Maybe AUDistMatrixPerIndepVar
     -> Maybe SUDistMatrixPerIndepVar
     -> Maybe SUDistMatrixPerIndepVar
     -> Double
     -> [DepVarName]
     -> [KernelOneDepVar]
     -> Permutation
     -> IO [SearchResultRow]
search algorithm indepVars
     maybeObsGridDists maybeObsObsDists maybeGridGridDists
     spatDistUnitScaling
     depVars kernelsPerDepVar
     (Permutation tempSamplingIteration obs grid maybeGridTrueDep searchDepVarPos) = do
    perDepVar <- case algorithm of
        GPR -> do
            -- gpr
            distsObsGrid <- case maybeObsGridDists of -- this could be refactored to be shorter
                Nothing -> do
                    auMatrixToFlat <$> calcObsGridDistances spatDistUnitScaling obs grid indepVars
                Just (AUDistMatrixPerIndepVar ms) ->
                    auMatrixToFlat . AUDistMatrixPerIndepVar <$>
                        forM indepVars (\name -> case lookup name ms of
                           Just m  -> pure (name, m)
                           Nothing -> calcObsGridOneDim spatDistUnitScaling obs grid name)
            distsObsObs <- case maybeObsObsDists of
                Nothing -> do
                     suMatrixToFlatHalf <$> calcObsObsDistances spatDistUnitScaling obs indepVars
                Just (SUDistMatrixPerIndepVar ms) ->
                    suMatrixToFlatHalf . SUDistMatrixPerIndepVar <$>
                        forM indepVars (\name -> case lookup name ms of
                           Just m  -> pure (name, m)
                           Nothing -> calcSUDistOneDim spatDistUnitScaling (\(Observation _ _ (HyperPos pos _) _) -> pos) obs name)
            distsGridGrid <- case maybeGridGridDists of
                Nothing -> do
                     suMatrixToFlatHalf <$> calcGridGridDistances spatDistUnitScaling grid indepVars
                Just (SUDistMatrixPerIndepVar ms) ->
                    suMatrixToFlatHalf . SUDistMatrixPerIndepVar <$>
                        forM indepVars (\name -> case lookup name ms of
                           Just m  -> pure (name, m)
                           Nothing -> calcSUDistOneDim spatDistUnitScaling id grid name)
            --putStrLn $ show $ VS.take 100 $ VS.reverse $ payload distsObsGrid
            --error "test"
            return $ zipWith (gpr obs grid maybeGridTrueDep distsObsGrid distsObsObs distsGridGrid searchDepVarPos) depVars kernelsPerDepVar
        KAS -> do
            -- kas
            distsObsGrid <- case maybeObsGridDists of
                Nothing -> do
                     auMatrixToFlat <$> calcObsGridDistances spatDistUnitScaling obs grid indepVars
                Just (AUDistMatrixPerIndepVar ms) ->
                    auMatrixToFlat . AUDistMatrixPerIndepVar <$>
                        forM indepVars (\name -> case lookup name ms of
                           Just m  -> pure (name, m)
                           Nothing -> calcObsGridOneDim spatDistUnitScaling obs grid name)
            return $ zipWith (kas obs maybeGridTrueDep distsObsGrid searchDepVarPos) depVars kernelsPerDepVar
    -- turn SSL to SSR
    let rawRows = concatMap (rowsForGridIdx perDepVar) [0 .. (V.length grid)-1]
    if isJust searchDepVarPos && isSpatioTemporal grid
    then return $ normaliseByTimeSlice rawRows
    else return rawRows
    where
        rowsForGridIdx :: [V.Vector SearchResultLong] -> Int -> [SearchResultRow]
        rowsForGridIdx perDepVar i =
            let resAtI = map (V.! i) perDepVar
                mkRow :: Maybe DepVarsPredPos -> [Maybe Double] -> SearchResultRow
                mkRow mSearchOne llsOne =
                    let truthLLs = map _sslGridLogLikelihood resAtI
                    in SSR { _ssrTempSampIter     = tempSamplingIteration
                        , _ssrIndepVarsPos     = grid V.! i
                        , _ssrDepVarName       = map _sslDepVarName resAtI
                        , _ssrLowerBound       = map _sslLowerBound resAtI
                        , _ssrMedian           = map _sslMedian     resAtI
                        , _ssrUpperBound       = map _sslUpperBound resAtI
                        , _ssrGridLogLikelihood = truthLLs
                        , _ssrGridAggLogLik     = sumIfAllJust truthLLs
                        , _ssrSearchPos        = mSearchOne
                        , _ssrLogLikelihood    = llsOne
                        , _ssrAggLogLikelihood = sumIfAllJust llsOne
                        , _ssrProbability      = Nothing
                        }
                -- extract per-depVar likelihoods at index j (safely)
                llsAt :: Int -> [Maybe Double]
                llsAt j = [ mv >>= (V.!? j) | mv <- map _sslLogLikelihood resAtI ]
                -- branches for absent/present search candidates
                depCount  = length perDepVar
                rowsNoSearch :: [SearchResultRow]
                rowsNoSearch = [ mkRow Nothing (replicate depCount Nothing) ]
                rowsWithSearch :: V.Vector DepVarsPredPos -> [SearchResultRow]
                rowsWithSearch svec = V.toList $ V.imap (\j sp -> mkRow (Just sp) (llsAt j)) svec
            in maybe rowsNoSearch rowsWithSearch searchDepVarPos
        sumIfAllJust :: [Maybe Double] -> Maybe Double
        sumIfAllJust xs = do
          ys <- sequence xs
          if null ys then Nothing else Just (sum ys)

-- normalisation mechanism
normaliseByTimeSlice :: [SearchResultRow] -> [SearchResultRow]
normaliseByTimeSlice rows =
    -- group all log-likelihoods per time slice
    let grouped = foldl' (\m row ->
                      case _ssrAggLogLikelihood row of
                        Just ll -> Map.insertWith (++) (makeKey row) [ll] m
                        Nothing -> Map.insertWith (++) (makeKey row) [] m
                   ) Map.empty rows
    -- compute log–sum–exp denom per time slice
        factors = Map.map (\logs ->
                     let maxLog = if null logs then 0 else maximum logs
                         denom  = sum [exp (l - maxLog) | l <- logs]
                     in (maxLog, denom)
                  ) grouped
    -- normalise each row within its time slice
        normRow row = case (_ssrAggLogLikelihood row, Map.lookup (makeKey row) factors) of
            (Just ll, Just (maxLog, denom)) | denom > 0 ->
                 row { _ssrProbability = Just $ exp (ll - maxLog) / denom }
            _ -> row { _ssrProbability = Nothing }
    in map normRow rows

makeKey :: SearchResultRow -> (DepVarsPredPos, Int)
makeKey row =
    let searchPos = case _ssrSearchPos row of
            Just x -> x
            _      -> error "impossible state"
        t = case _ssrIndepVarsPos row of
            IndepSpatTempPos (SpatTempPos _ (TempPos x)) -> x
            _ -> error "impossible state"
    in (searchPos, t)

-- permutation mechanism
data Permutation = Permutation {
      _permTempSamplingIteration :: Int
    , _permObs                   :: V.Vector Observation
    , _permIndepPredGrid         :: V.Vector IndepVarsPos
    , _permGridTrueDep           :: Maybe (V.Vector DepVarsPos)
    , _permDepSearchGrid         :: Maybe (V.Vector DepVarsPredPos)
} deriving (Show)

createPermutations :: V.Vector Observation -> Maybe TempSampleMatrix
                   -> V.Vector IndepVarsPos -> Maybe (V.Vector DepVarsPredPos) -> Maybe [AbsRelTempPos]
                   -> [Permutation]
createPermutations obs maybeTempSampleMatrix indepPredGrid depSearchGrid maybeTempGrid = do
    -- apply temp resampling to obs
    tempSamp <- [0..(nrTempSamples maybeTempSampleMatrix - 1)]
    let modObs = V.map (applyTempSamp maybeTempSampleMatrix tempSamp) obs
    -- grid construction including time
    (indepPredPos, depSearchPos) <- splitDataByTempGrid maybeTempGrid indepPredGrid depSearchGrid
    -- assemble permutations
    return $ Permutation tempSamp modObs indepPredPos Nothing depSearchPos

nrTempSamples :: Maybe TempSampleMatrix -> Int
nrTempSamples Nothing                         = 1
nrTempSamples (Just (TempSampleMatrix n _ _)) = n

applyTempSamp :: Maybe TempSampleMatrix -> Int -> Observation -> Observation
applyTempSamp (Just m) i
    obs@(Observation i1 i2 (HyperPos (IndepSpatTempPos (SpatTempPos i3 _)) i4) i5) =
    let obsIndex = getIndex obs
        newage = lookUpTempSample m i obsIndex
    in Observation i1 i2 (HyperPos (IndepSpatTempPos (SpatTempPos i3 (TempPos newage))) i4) i5
applyTempSamp _ _ obs = obs

splitDataByTempGrid :: Maybe [AbsRelTempPos] -> V.Vector IndepVarsPos -> Maybe (V.Vector DepVarsPredPos)
                    -> [(V.Vector IndepVarsPos, Maybe (V.Vector DepVarsPredPos))]
splitDataByTempGrid Nothing indepPredGrid depSearchGrid = [(indepPredGrid, depSearchGrid)]
splitDataByTempGrid (Just absRelTempPos) indepPredGrid depSearchGrid =
    let spatPos = V.mapMaybe (
            \pos -> case pos of
                  IndepSpatTempPos (SpatTempPos s _) -> Just s
                  _                                  -> Nothing
            ) indepPredGrid
    in concat $ mapMaybe (\t -> build t spatPos depSearchGrid) absRelTempPos
  where
    build :: AbsRelTempPos -> V.Vector SpatPos -> Maybe (V.Vector DepVarsPredPos)
          -> Maybe [(V.Vector IndepVarsPos, Maybe (V.Vector DepVarsPredPos))]
    build (AbsTempPos yearBCAD) spatPos depGrid =
      let indepVarsPos = V.map (\s -> IndepSpatTempPos (SpatTempPos s (TempPos yearBCAD))) spatPos
      in Just [(indepVarsPos, depGrid)]
    build (RelTempPos yearDist) spatPos (Just searchGrid) =
      let refAge = V.toList $ V.mapMaybe getObsAge searchGrid
          indepVarsPos = for refAge $ \r -> V.map (\s -> IndepSpatTempPos (SpatTempPos s (TempPos $ r + yearDist))) spatPos
      in Just $ zip indepVarsPos (map Just $ V.group searchGrid)
    build _ _ _ = Nothing

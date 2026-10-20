{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module LocEst.CLI.Search where

import           LocEst.CoreAlgorithms
import           LocEst.Distance
import           LocEst.Distributions
import           LocEst.Parsers
import           LocEst.Types
import           LocEst.TypesFlat
import           LocEst.Utils

import           Conduit                  (liftIO)
import           Control.DeepSeq          (force)
import           Control.Exception        (evaluate)
import           Data.Conduit             ((.|))
import qualified Data.Conduit             as Con
import qualified Data.Conduit.Combinators as ConC
import qualified Data.Conduit.List        as ConL
import           Data.Foldable            (foldl')
import           Data.List                (intercalate, transpose, sortOn)
import qualified Data.Map.Strict          as Map
import           Data.Maybe               (isJust, isNothing, catMaybes, mapMaybe)
import qualified Data.Vector              as V
import           System.IO                (hPutStrLn, stderr)
import Data.Ord (Down(..))
import qualified Data.List.NonEmpty     as NE

data SearchOptions = SearchOptions
    { _searchInObservationFile   :: FilePath
    , _searchInTempSampFile      :: Maybe FilePath
    , _searchInIndepPredGridFile :: FilePath
    , _searchInTempGrid          :: Maybe [AbsRelTempPos]
    , _searchInDepSearchGrid     :: Maybe DepVarsPredGridSettings
    , _searchAlgorithm           :: KernelDefinition
    , _searchInObsObsDistFile    :: Maybe FilePath
    , _searchInObsGridDistFile   :: Maybe FilePath
    -- , _searchInGridGridDistFile  :: Maybe FilePath
    , _searchTopNObs             :: Int
    , _searchOutFile             :: Maybe FilePath
    }

data InterpolationEnv = InterpolationEnv
    { _ieSpatDistUnitScaling :: Double
    , _ieKernelDefinition    :: KernelDefinition
    , _ieTopNObs             :: Int
    , _ieIndepVars           :: [IndepVarName]
    , _ieObsGridDists        :: Maybe CrossDistMatrixPerIndepVar
    , _ieObsObsDists         :: Maybe SelfDistMatrixPerIndepVar
    }

runSearch :: SearchOptions -> Double -> IO ()
runSearch (SearchOptions
    inObsFile maybeTempSampFile inIndepVarsPredGridFile maybeTempGrid
    inMaybeDepSearchGrid kernDef
    maybeObsObsDistFile maybeObsGridDistFile -- maybeGridGridDistFile
    topNObs outFile
    ) spatDistUnitScaling = do
    -- algorithm settings
    let algorithm = _kdefAlgorithm kernDef
        depVars   = getKeys kernDef
        indepVars = case _kdefPerDepVar kernDef of
            (k:_) -> getKeys (_kodvLengths k)
            []    -> throwL "runSearch: empty KernelDefinition (this should be impossible)"
    hPutStrLn stderr $ "Algorithm: " ++ show algorithm
    hPutStrLn stderr $ "Dependent variables: " ++ intercalate ", " depVars
    hPutStrLn stderr $ "Independent variables: " ++ intercalate ", " indepVars
    -- read observations
    !obs <- filterVarsInObs depVars indepVars <$> readObservations inObsFile
    let nObs = V.length obs
    hPutStrLn stderr $ "Number of observations: " ++ show nObs
    -- read temporal resampling iterations
    !maybeTempSamp <- traverse (readTempSamp obs) maybeTempSampFile
    -- read indepVar prediction grid positions
    !indepPredGrid <- V.map (filterVarsInIndepVarsPos indepVars) <$> readIndepVarsPos inIndepVarsPredGridFile
    let nGrid = V.length indepPredGrid
    hPutStrLn stderr $ "Number of grid positions: " ++ show nGrid
    -- read depVar search grid
    !depSearchGrid <- traverse (readDepVarsPredGrid depVars indepVars) inMaybeDepSearchGrid
    -- read distances
    !obsGridDistances  <- traverse (readCrossDistMulti nObs nGrid) maybeObsGridDistFile
    !obsObsDistances   <- traverse (readSelfDistMulti nObs) maybeObsObsDistFile
    -- !gridGridDistances <- traverse (readSelfDistMulti nGrid) maybeGridGridDistFile
    -- compile interpolation setup
    let interpolationEnv = InterpolationEnv {
          _ieSpatDistUnitScaling = spatDistUnitScaling
        , _ieKernelDefinition    = kernDef
        , _ieTopNObs             = topNObs
        , _ieIndepVars           = indepVars
        , _ieObsGridDists        = obsGridDistances
        , _ieObsObsDists         = obsObsDistances
        }
    -- run interpolation and search
    hPutStrLn stderr "Running interpolation"
    let tempSamples = tempSampleAxis obs maybeTempSamp
        timeSlices = if canMergeTempGrid maybeTempGrid maybeObsGridDistFile
                     then mergeDataByAbsTempGrid maybeTempGrid indepPredGrid depSearchGrid
                     else splitDataByTempGrid maybeTempGrid indepPredGrid depSearchGrid
        nTempSamples = length tempSamples
        nrWorkItems  = length timeSlices * nTempSamples
    Con.runConduitRes $
           ConC.yieldMany timeSlices
        .| ConL.concatMap (\ts -> map (ts,) tempSamples)
        .| ConL.mapM (liftIO . interpol interpolationEnv)
        .| progress 1 (Just nrWorkItems)
        .| ConL.chunksOf nTempSamples
        .| ConL.mapMaybe NE.nonEmpty -- chunksOf returns a nonEmpty list
        .| ConL.map aggregateTempSamples
        .| ConL.map searchForAllGridPoints
        .| ConL.concatMap normaliseFinishedTimeSlice
        .| sinkNamedCSV outFile
    hPutStrLn stderr "Done"

interpol :: InterpolationEnv -> (TimeSlice, V.Vector Observation) -> IO (TimeSlice, [InterpolResultWide])
interpol env (timeSlice@(grid, _), obs') = do
    hPutStrLn stderr "Started work package..."
    perDepVar <- interpolPerDepVar env obs' grid Nothing
    pure (timeSlice, interpolLongToWide (_ieKernelDefinition env) grid perDepVar)

interpolPerDepVar
    :: InterpolationEnv
    -> V.Vector Observation
    -> V.Vector IndepVarsPos
    -> Maybe (V.Vector DepVarsPos)
    -> IO [V.Vector InterpolResultLong]
interpolPerDepVar env obs grid maybeGridTrueDep = do
    let spatDistUnitScaling = _ieSpatDistUnitScaling env
        kernDef             = _ieKernelDefinition env
        algorithm           = _kdefAlgorithm kernDef
        topNObs             = _ieTopNObs env
        indepVars           = _ieIndepVars env
        maybeObsGridDists   = _ieObsGridDists env
        maybeObsObsDists    = _ieObsObsDists env
        kernelsPerDepVar    = _kdefPerDepVar kernDef
    hPutStrLn stderr "Computing pairwise distances..."
    -- obs-grid dists are always needed
    distsObsGrid <- case maybeObsGridDists of
        Nothing -> crossDistMatrixToFlat <$> calcObsGridDistances spatDistUnitScaling obs grid indepVars
        Just (CrossDistMatrixPerIndepVar ms) ->
            crossDistMatrixToFlat . CrossDistMatrixPerIndepVar <$>
                forM indepVars (\name -> case lookup name ms of
                   Just m  -> pure (name, m)
                   Nothing -> calcObsGridOneDim spatDistUnitScaling obs grid name)
    case algorithm of
        GPR -> do
            distsObsObs <- case maybeObsObsDists of
                Nothing -> selfDistMatrixToFlatHalf <$> calcObsObsDistances spatDistUnitScaling obs indepVars
                Just (SelfDistMatrixPerIndepVar ms) ->
                    selfDistMatrixToFlatHalf . SelfDistMatrixPerIndepVar <$>
                        forM indepVars (\name -> case lookup name ms of
                           Just m  -> pure (name, m)
                           Nothing -> calcSelfDistOneDim spatDistUnitScaling (\(Observation _ _ (HyperPos pos _) _) -> pos) obs name)
            -- distsGridGrid <- case maybeGridGridDists of
            --     Nothing -> selfDistMatrixToFlatHalf <$> calcGridGridDistances spatDistUnitScaling grid indepVars
            --     Just (SelfDistMatrixPerIndepVar ms) ->
            --         selfDistMatrixToFlatHalf . SelfDistMatrixPerIndepVar <$>
            --             forM indepVars (\name -> case lookup name ms of
            --                Just m  -> pure (name, m)
            --                Nothing -> calcSelfDistOneDim spatDistUnitScaling id grid name)
            hPutStrLn stderr "Interpolating..."
            forM kernelsPerDepVar $ \kernel -> do
                let depVar = _kodvDepVarName kernel
                hPutStrLn stderr depVar
                let res = gpr obs grid maybeGridTrueDep distsObsGrid distsObsObs topNObs depVar kernel
                evaluate (force res)
        KAS -> do
            hPutStrLn stderr "Interpolating..."
            forM kernelsPerDepVar $ \kernel -> do
                let depVar = _kodvDepVarName kernel
                hPutStrLn stderr depVar
                let res = kas obs maybeGridTrueDep distsObsGrid topNObs depVar kernel
                evaluate (force res)

interpolLongToWide
    :: KernelDefinition
    -> V.Vector IndepVarsPos
    -> [V.Vector InterpolResultLong]
    -> [InterpolResultWide]
interpolLongToWide kernDef grid perDepVar = map wideGridIdx [0 .. V.length grid - 1]
  where
    wideGridIdx :: Int -> InterpolResultWide
    wideGridIdx i =
      let resAtI = map (V.! i) perDepVar
      in IRW { _irwKernDef          = kernDef
             , _irwGridIndepVarsPos = grid V.! i
             , _irwGridDepVarsPos   = map _irlGridDepVarsPos resAtI
             , _irwTopObsIDs        = map _irlTopObsIDs resAtI
             , _irwDepVarName       = map _irlDepVarName resAtI
             , _irwPredDist         = map _irlPredDist resAtI
             }

aggregateTempSamples :: NE.NonEmpty (TimeSlice, [InterpolResultWide]) -> [(TimeSlice, InterpolResultWide)]
aggregateTempSamples xs = mapMaybe combineColumn $ transpose $ map snd $ NE.toList xs
    where
        timeSlice = fst (NE.head xs)
        combineColumn rows = (timeSlice,) . combineTempResamplingRuns <$> NE.nonEmpty rows

combineTempResamplingRuns :: NE.NonEmpty InterpolResultWide -> InterpolResultWide
combineTempResamplingRuns rows =
    let r0 = NE.head rows
    in r0 { _irwTopObsIDs = map consensusTopObs $ transpose $ map _irwTopObsIDs $ NE.toList rows
          , _irwPredDist  = map mix . transpose $ map _irwPredDist $ NE.toList rows
          }

-- combine top observations across temporal resampling runs
consensusTopObs :: [Maybe String] -> Maybe String
consensusTopObs xs
    | null rankings = Nothing
    | otherwise = Just . intercalate ";" . take nOut . map fst . sortOn rankingKey $ Map.toList stats
  where
    rankings = filter (not . null) $ map splitTopObs $ catMaybes xs
    -- normally every ranking has the same length.
    nOut = maximum (map length rankings)
    -- for each obs, retain occurrence count and total rank
    stats :: Map.Map String (Int, Int)
    stats = foldl' addRanking Map.empty rankings
    addRanking m ids = foldl' addOne m (zip [1..] ids)
    addOne m (rank, obsID) =
        Map.insertWith
            (\(newCount, newRank) (oldCount, oldRank) ->
                (newCount + oldCount, newRank + oldRank))
            obsID
            (1, rank)
            m
    -- higher occurrence count first, then lower mean rank
    rankingKey (obsID, (count, totalRank)) =
        ( Down count
        , fromIntegral totalRank / fromIntegral count :: Double
        , obsID
        )
    splitTopObs :: String -> [String]
    splitTopObs "" = []
    splitTopObs s = case break (== ';') s of
                      (x, [])       -> [x]
                      (x, _ : rest) -> x : splitTopObs rest

searchForAllGridPoints :: [(TimeSlice, InterpolResultWide)] -> [(TimeSlice, SearchResultWide)]
searchForAllGridPoints =  concatMap (\(ts@(_, maybeSearchGrid), irw) -> map (ts,) (search maybeSearchGrid irw))

search :: Maybe (V.Vector DepVarsPredPos) -> InterpolResultWide -> [SearchResultWide]
search Nothing irw = [searchOne Nothing irw]
search (Just searchGrid) irw = map (\x -> searchOne (Just x) irw) (V.toList searchGrid)

searchOne :: Maybe DepVarsPredPos -> InterpolResultWide -> SearchResultWide
searchOne maybeSearchPos irw =
    let depNames  = _irwDepVarName irw
        predDists = _irwPredDist irw
        gridDeps  = _irwGridDepVarsPos irw
        gridLLs =
            [ gridLL dist depName maybeGridDep
            | (dist, depName, maybeGridDep) <- zip3 predDists depNames gridDeps
            ]
        searchLLs =
            [ searchLL dist depName maybeSearchPos
            | (dist, depName) <- zip predDists depNames
            ]
    in SRW { _srwKernDef           = _irwKernDef irw
           , _srwGridIndepVarsPos  = _irwGridIndepVarsPos irw
           , _srwTopObsIDs         = _irwTopObsIDs irw
           , _srwDepVarName        = depNames
           , _srwPredDist          = predDists
           , _srwGridLogLikelihood = gridLLs
           , _srwGridAggLogLik     = sumIfAllJust gridLLs
           , _srwSearchPos         = maybeSearchPos
           , _srwLogLikelihood     = searchLLs
           , _srwAggLogLikelihood  = sumIfAllJust searchLLs
           , _srwProbability       = Nothing
           }

gridLL :: Either String PredDist -> DepVarName -> Maybe DepVarsPos -> Maybe Double
gridLL _ _ Nothing = Nothing
gridLL (Left _) _ (Just _) = Just (-inf)
gridLL (Right dist) depName (Just depPos) = Just $ predLogDensity dist (lookupUnsafe depPos depName)

searchLL :: Either String PredDist -> DepVarName -> Maybe DepVarsPredPos -> Maybe Double
searchLL _ _ Nothing =  Nothing
searchLL (Left _) _ (Just _) = Just (-inf)
searchLL (Right dist) depName (Just searchPos) = Just $ predLogDensity dist (getDepVarsPos2 depName searchPos)

sumIfAllJust :: [Maybe Double] -> Maybe Double
sumIfAllJust xs = do
    ys <- sequence xs
    if null ys then Nothing else Just (sum ys)

normaliseFinishedTimeSlice :: [(TimeSlice, SearchResultWide)] -> [SearchResultWide]
normaliseFinishedTimeSlice [] = []
normaliseFinishedTimeSlice xs@(((grid, searchDepVarPos), _) : _) =
    if isJust searchDepVarPos && isSpatioTemporal grid
    then normaliseByTimeSlice $ map snd xs
    else map snd xs

-- normalisation mechanism
normaliseByTimeSlice :: [SearchResultWide] -> [SearchResultWide]
normaliseByTimeSlice rows =
    -- group all log-likelihoods per time slice
    let grouped = foldl' (\m row ->
                      case _srwAggLogLikelihood row of
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
        normRow row = case (_srwAggLogLikelihood row, Map.lookup (makeKey row) factors) of
            (Just ll, Just (maxLog, denom)) | denom > 0 ->
                 row { _srwProbability = Just $ exp (ll - maxLog) / denom }
            _ -> row { _srwProbability = Nothing }
    in map normRow rows

makeKey :: SearchResultWide -> (DepVarsPredPos, Int)
makeKey row =
    let searchPos = case _srwSearchPos row of
            Just x -> x
            _      -> error "impossible state"
        t = case _srwGridIndepVarsPos row of
            IndepSpatTempPos (SpatTempPos _ (TempPos x)) -> x
            _ -> error "impossible state"
    in (searchPos, t)

-- temporal resampling over observations
tempSampleAxis :: V.Vector Observation -> Maybe TempSampleMatrix -> [V.Vector Observation]
tempSampleAxis obs m = [ V.map (applyTempSamp m ix) obs | ix <- [0 .. nrTempSamples m - 1] ]

applyTempSamp :: Maybe TempSampleMatrix -> Int -> Observation -> Observation
applyTempSamp (Just m) i
    obs@(Observation i1 i2 (HyperPos (IndepSpatTempPos (SpatTempPos i3 _)) i4) i5) =
    let obsIndex = getIndex obs
        newage   = lookUpTempSample m i obsIndex
    in Observation i1 i2 (HyperPos (IndepSpatTempPos (SpatTempPos i3 (TempPos newage))) i4) i5
applyTempSamp _ _ obs = obs

-- expand independent-variable grid by requested time points
type TimeSlice = (V.Vector IndepVarsPos, Maybe (V.Vector DepVarsPredPos))

splitDataByTempGrid
    :: Maybe [AbsRelTempPos]
    -> V.Vector IndepVarsPos
    -> Maybe (V.Vector DepVarsPredPos)
    -> [TimeSlice]
splitDataByTempGrid Nothing indepPredGrid maybeDepSearchGrid =
    [(indepPredGrid, maybeDepSearchGrid)]
splitDataByTempGrid (Just tempPos) indepPredGrid maybeDepSearchGrid =
    let spatGrid = V.map spatPosFromIndepVarsPos indepPredGrid
    in concatMap (expandOne spatGrid maybeDepSearchGrid) tempPos

canMergeTempGrid :: Maybe [AbsRelTempPos] -> Maybe FilePath -> Bool
canMergeTempGrid maybeTempGrid maybeObsGridDistFile =
    case maybeTempGrid of
      Nothing -> True
      Just xs -> all isAbsTempPos xs && isNothing maybeObsGridDistFile
      -- important: merging changes the obs-grid distance shape

mergeDataByAbsTempGrid
    :: Maybe [AbsRelTempPos]
    -> V.Vector IndepVarsPos
    -> Maybe (V.Vector DepVarsPredPos)
    -> [TimeSlice]
mergeDataByAbsTempGrid Nothing indepPredGrid maybeDepSearchGrid =
    [(indepPredGrid, maybeDepSearchGrid)]
mergeDataByAbsTempGrid (Just tempPos) indepPredGrid maybeDepSearchGrid =
    let spatGrid = V.map spatPosFromIndepVarsPos indepPredGrid
        grids = [ makeGridAtTime spatGrid yearBCAD | AbsTempPos yearBCAD <- tempPos ]
    in if null grids
       then []
       else [(V.concat grids, maybeDepSearchGrid)]

expandOne
    :: V.Vector SpatPos
    -> Maybe (V.Vector DepVarsPredPos)
    -> AbsRelTempPos
    -> [TimeSlice]
expandOne spatGrid maybeDepSearchGrid = \case
    AbsTempPos yearBCAD -> [(makeGridAtTime spatGrid yearBCAD, maybeDepSearchGrid)]
    RelTempPos yearDist ->
        case maybeDepSearchGrid of
          Nothing -> []
          Just depGrid ->
              let refAges = V.toList (V.mapMaybe getObsAge depGrid)
                  grids   = [ makeGridAtTime spatGrid (r + yearDist) | r <- refAges ]
                  deps    = map Just (V.group depGrid) -- depends on pre-arranged ordering
              in zip grids deps

makeGridAtTime :: V.Vector SpatPos -> YearBCAD -> V.Vector IndepVarsPos
makeGridAtTime spatGrid year = V.map (\s -> IndepSpatTempPos (SpatTempPos s (TempPos year))) spatGrid

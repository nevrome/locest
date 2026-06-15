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
import           Data.List                (intercalate, transpose)
import qualified Data.Map.Strict          as Map
import           Data.Maybe               (isJust)
import qualified Data.Vector              as V
import           System.IO                (hPutStrLn, stderr)

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
        kernels   = getValues kernDef
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
    -- run interpolation and search
    hPutStrLn stderr "Running interpolation"
    let tempSamples = tempSampleAxis obs maybeTempSamp
        timeSlices  = splitDataByTempGrid maybeTempGrid indepPredGrid depSearchGrid
        nTempSamples = length tempSamples
        nrWorkItems  = length timeSlices * nTempSamples
    Con.runConduitRes $
           ConC.yieldMany timeSlices
        .| ConL.concatMap (\ts -> map (ts,) tempSamples)
        .| ConL.mapM (
            liftIO . interpol
                spatDistUnitScaling algorithm kernDef
                topNObs indepVars obsGridDistances
                obsObsDistances depVars kernels
           )
        .| progress 1 (Just nrWorkItems)
        .| ConL.chunksOf nTempSamples
        .| ConL.map aggregateTempSamples
        .| ConL.map searchForAllGridPoints
        .| ConL.concatMap normaliseFinishedTimeSlice
        .| sinkNamedCSV outFile
    hPutStrLn stderr "Done"

interpol
    :: Double
    -> Algorithm
    -> KernelDefinition
    -> Int
    -> [IndepVarName]
    -> Maybe CrossDistMatrixPerIndepVar
    -> Maybe SelfDistMatrixPerIndepVar
    -> [DepVarName]
    -> [KernelOneDepVar]
    -> (TimeSlice, V.Vector Observation)
    -> IO (TimeSlice, [InterpolResultWide])
interpol spatDistUnitScaling algorithm kernDef topNObs indepVars
    maybeObsGridDists maybeObsObsDists depVars kernelsPerDepVar
    (timeSlice@(grid, _), obs') = do
    perDepVar <- interpolPerDepVar
        spatDistUnitScaling
        algorithm
        topNObs
        indepVars
        maybeObsGridDists
        maybeObsObsDists
        depVars
        kernelsPerDepVar
        obs'
        grid
        Nothing
    perDepVar' <- evaluate (force perDepVar)
    pure (timeSlice, interpolLongToWide kernDef grid perDepVar')

interpolPerDepVar
    :: Double
    -> Algorithm
    -> Int
    -> [IndepVarName]
    -> Maybe CrossDistMatrixPerIndepVar
    -> Maybe SelfDistMatrixPerIndepVar
    -- -> Maybe SelfDistMatrixPerIndepVar
    -> [DepVarName]
    -> [KernelOneDepVar]
    -> V.Vector Observation
    -> V.Vector IndepVarsPos
    -> Maybe (V.Vector DepVarsPos)
    -> IO [V.Vector InterpolResultLong]
interpolPerDepVar spatDistUnitScaling algorithm topNObs indepVars
     maybeObsGridDists maybeObsObsDists -- maybeGridGridDists
     depVars kernelsPerDepVar
     obs grid maybeGridTrueDep = do
    -- obs-grid dists are always needed
    distsObsGrid <- case maybeObsGridDists of
        Nothing -> do
             crossDistMatrixToFlat <$> calcObsGridDistances spatDistUnitScaling obs grid indepVars
        Just (CrossDistMatrixPerIndepVar ms) ->
            crossDistMatrixToFlat . CrossDistMatrixPerIndepVar <$>
                forM indepVars (\name -> case lookup name ms of
                   Just m  -> pure (name, m)
                   Nothing -> calcObsGridOneDim spatDistUnitScaling obs grid name)
    case algorithm of
        GPR -> do
            distsObsObs <- case maybeObsObsDists of
                Nothing -> do
                     selfDistMatrixToFlatHalf <$> calcObsObsDistances spatDistUnitScaling obs indepVars
                Just (SelfDistMatrixPerIndepVar ms) ->
                    selfDistMatrixToFlatHalf . SelfDistMatrixPerIndepVar <$>
                        forM indepVars (\name -> case lookup name ms of
                           Just m  -> pure (name, m)
                           Nothing -> calcSelfDistOneDim spatDistUnitScaling (\(Observation _ _ (HyperPos pos _) _) -> pos) obs name)
            -- distsGridGrid <- case maybeGridGridDists of
            --     Nothing -> do
            --          selfDistMatrixToFlatHalf <$> calcGridGridDistances spatDistUnitScaling grid indepVars
            --     Just (SelfDistMatrixPerIndepVar ms) ->
            --         selfDistMatrixToFlatHalf . SelfDistMatrixPerIndepVar <$>
            --             forM indepVars (\name -> case lookup name ms of
            --                Just m  -> pure (name, m)
            --                Nothing -> calcSelfDistOneDim spatDistUnitScaling id grid name)
            return $ zipWith (gpr obs grid maybeGridTrueDep distsObsGrid distsObsObs topNObs) depVars kernelsPerDepVar
        KAS -> do
            return $ zipWith (kas obs maybeGridTrueDep distsObsGrid topNObs) depVars kernelsPerDepVar

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

aggregateTempSamples :: [(TimeSlice, [InterpolResultWide])] -> [(TimeSlice, InterpolResultWide)]
aggregateTempSamples [] = throwL "aggregateTempSamples: empty"
aggregateTempSamples xs@((timeSlice, _) : _) =
    map ((timeSlice,) . combineTempResamplingRuns) (transpose $ map snd xs)

combineTempResamplingRuns :: [InterpolResultWide] -> InterpolResultWide
combineTempResamplingRuns [] = throwL "combineTempResamplingRuns: empty"
combineTempResamplingRuns rows@(r0:_) =
    r0 { -- TODO: topObs also differ between resampling runs and must be aggregated somehow...
         -- _irwTopObsIDs         = replicate depCount Nothing
         _irwPredDist          = map mix . transpose $ map _irwPredDist rows
       }

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

makeGridAtTime :: V.Vector SpatPos -> YearBCAD -> V.Vector IndepVarsPos
makeGridAtTime spatGrid year = V.map (\s -> IndepSpatTempPos (SpatTempPos s (TempPos year))) spatGrid

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

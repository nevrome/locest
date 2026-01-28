{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TupleSections #-}

module LocEst.Distance where

import           LocEst.Types
import           LocEst.TypesFlat

import           Data.Foldable                (forM_)
import           Data.Maybe                   (listToMaybe)
import qualified Data.Vector                  as V
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Storable.Mutable as VSM
import           LocEst.Utils                 (throwL)

calcObsGridDistances :: Double -> V.Vector Observation -> V.Vector IndepVarsPos -> [IndepVarName] -> IO CrossDistMatrixPerIndepVar
calcObsGridDistances spatScale obs grid varsToCompute = do
    let indepVarNames = case grid V.! 0 of
          IndepSpatTempPos _                            -> ["space", "time"]
          IndepArbitraryDimPos (ValuesPerIndepVar ns _) -> V.toList ns
    let selected = filter (`elem` varsToCompute) indepVarNames
    mats <- mapM (calcObsGridOneDim spatScale obs grid) selected
    pure (CrossDistMatrixPerIndepVar mats)

calcObsGridOneDim :: Double -> V.Vector Observation -> V.Vector IndepVarsPos -> IndepVarName -> IO (IndepVarName, CrossDistMatrix)
calcObsGridOneDim spatScale obs grid varName = do
    case grid V.! 0 of
        IndepSpatTempPos _ ->
            case varName of
                "space" -> do
                  let obsPos  = V.map (spatPosFromIndepVarsPos . posFromObs) obs
                      gridPos = V.map spatPosFromIndepVarsPos grid
                  fmap (varName,) (computeSpaceCrossDistMatrix spatScale obsPos gridPos)
                "time"  -> do
                  let obsPos  = V.map (tempPosFromIndepVarsPos . posFromObs) obs
                      gridPos = V.map tempPosFromIndepVarsPos grid
                  fmap (varName,) (computeTimeCrossDistMatrix obsPos gridPos)
                _       -> error ("Unknown space-time variable: " ++ varName)
        IndepArbitraryDimPos (ValuesPerIndepVar names _) ->
            case V.elemIndex varName names of
                Just ix -> do
                    let obsPos  = V.map (anyPosFromIndepVarsPos . posFromObs) obs
                        gridPos = V.map anyPosFromIndepVarsPos grid
                    fmap (varName,) (computeArbitraryCrossDistMatrix ix obsPos gridPos)
                Nothing -> error ("Unknown arbitrary variable: " ++ varName)

computeSpaceCrossDistMatrix :: Double -> V.Vector SpatPos -> V.Vector SpatPos -> IO CrossDistMatrix
computeSpaceCrossDistMatrix spatScale obs grid = do
    let nrObs  = V.length obs
        nrGrid = V.length grid
        !nTot  = nrGrid * nrObs
    mv <- VSM.new nTot
    forM_ [0 .. nrGrid-1] $ \gy -> do
        -- cache grid point once per outer loop
        let !s2    = V.unsafeIndex grid gy
            !base  = gy * nrObs
        forM_ [0 .. nrObs-1] $ \ox -> do
            let !s1 = V.unsafeIndex obs ox
                !d  = spatialDistSpatPos s1 s2 * spatScale
            VSM.unsafeWrite mv (base + ox) d
    frozen <- VS.unsafeFreeze mv
    pure (CrossDistMatrix nrObs nrGrid frozen)

computeTimeCrossDistMatrix :: V.Vector TempPos -> V.Vector TempPos -> IO CrossDistMatrix
computeTimeCrossDistMatrix obs grid = do
    let nrObs  = V.length obs
        nrGrid = V.length grid
        !nTot  = nrGrid * nrObs
    mv <- VSM.new nTot
    forM_ [0 .. nrGrid-1] $ \gy -> do
        let !t2   = V.unsafeIndex grid gy
            !base = gy * nrObs
        forM_ [0 .. nrObs-1] $ \ox -> do
            let !t1 = V.unsafeIndex obs ox
                !d  = temporalDistTempPos t1 t2
            VSM.unsafeWrite mv (base + ox) d
    frozen <- VS.unsafeFreeze mv
    pure (CrossDistMatrix nrObs nrGrid frozen)

computeArbitraryCrossDistMatrix
    :: Int
    -> V.Vector (VS.Vector Double) -- obs positions
    -> V.Vector (VS.Vector Double) -- grid positions
    -> IO CrossDistMatrix
computeArbitraryCrossDistMatrix ix obs grid = do
    let nrObs  = V.length obs
        nrGrid = V.length grid
        !nTot  = nrGrid * nrObs
    mv <- VSM.new nTot
    forM_ [0 .. nrGrid-1] $ \gy -> do
        let !vs2  = V.unsafeIndex grid gy
            !x2   = VS.unsafeIndex vs2 ix -- cache grid coordinate once
            !base = gy * nrObs
        forM_ [0 .. nrObs-1] $ \ox -> do
            let !vs1 = V.unsafeIndex obs ox
                !x1  = VS.unsafeIndex vs1 ix
                !d = abs (x1 - x2)
            VSM.unsafeWrite mv (base + ox) d
    frozen <- VS.unsafeFreeze mv
    pure (CrossDistMatrix nrObs nrGrid frozen)

crossDistMatrixToFlat :: CrossDistMatrixPerIndepVar -> IndepVarsDistFlat
crossDistMatrixToFlat cdmPerIndepVar =
    let mats = getValues cdmPerIndepVar
        matsV = V.fromList mats -- convert to vector for O(1) indexing
        stride = V.length matsV
        nRows = _cdmNrRows (V.head matsV)
        nCols = _cdmNrCols (V.head matsV)
        total = nRows * nCols
        -- compute tag once (all elements are identical)
        firstTag = case listToMaybe (getKeys cdmPerIndepVar) of
                     Just "space" -> False
                     _            -> True
        tagsVec = VS.replicate total firstTag
        -- precompute all matrix vectors
        matVecs = V.map _cdmMatrix matsV
        -- build payload without per-element divMod
        payloadVec = VS.create $ do
            mv <- VSM.unsafeNew (total * stride)
            V.iforM_ matVecs $ \dimIx vec ->
                VS.iforM_ vec $ \idx val ->
                    VSM.unsafeWrite mv (idx * stride + dimIx) val
            return mv
    in IndepVarsDistFlat tagsVec payloadVec stride

-- TODO: do scaling more elegantly: Could consider any variable, not just space and time
mergeDistsIndepVar :: (Double, Double) -> SelfDistMatrixPerIndepVar -> IO SelfDistMatrixPerIndepVar
mergeDistsIndepVar (spaceScale, timeScale) (SelfDistMatrixPerIndepVar ms) = do
    case ms of
        [] -> error "mergeDists: no matrices to merge"
        x:_  -> do
          -- all half matrices should have the same length
          let nHalf = VS.length (let (SelfDistMatrix v) = snd x in v)
          mv <- VSM.new nHalf
          forM_ [0..nHalf-1] $ \i -> do
              -- sum-of-squares accumulator
              let ssq = foldl' (\acc (name, SelfDistMatrix v) ->
                                   let scale | name == "space" = spaceScale
                                             | name == "time"  = timeScale
                                             | otherwise       = 1.0
                                   in acc + (v VS.! i / scale) ** 2
                               ) 0.0 ms
              VSM.write mv i (sqrt ssq)
          distsMerged <- VS.unsafeFreeze mv
          pure $ SelfDistMatrixPerIndepVar [("acrossIndep", SelfDistMatrix distsMerged)]

mergeDistsDepVar :: SelfDistMatrixPerIndepVar -> IO SelfDistMatrixPerIndepVar
mergeDistsDepVar (SelfDistMatrixPerIndepVar ms) = do
    case ms of
        [] -> error "mergeDists: no matrices to merge"
        x:_  -> do
          -- all half matrices should have the same length
          let nHalf = VS.length (let (SelfDistMatrix v) = snd x in v)
          mv <- VSM.new nHalf
          forM_ [0..nHalf-1] $ \i -> do
              -- sum-of-squares accumulator
              let ssq = foldl' (\acc (_, SelfDistMatrix v) -> acc + (v VS.! i) ** 2) 0.0 ms
              VSM.write mv i (sqrt ssq)
          distsMerged <- VS.unsafeFreeze mv
          pure $ SelfDistMatrixPerIndepVar [("acrossDep", SelfDistMatrix distsMerged)]

calcObsObsDistDepVar :: V.Vector Observation -> [DepVarName] -> IO SelfDistMatrixPerIndepVar
calcObsObsDistDepVar obs varsToCompute = do
    let depVarNames = getKeys $ depVarPosFromObs (V.head obs)
        selected = filter (`elem` varsToCompute) depVarNames
    mats <- mapM (calcSelfDistOneDimDepVar (\(Observation _ _ (HyperPos _ pos) _) -> pos) obs) selected
    pure (SelfDistMatrixPerIndepVar mats)

calcSelfDistOneDimDepVar :: (a -> DepVarsPos) -> V.Vector a -> DepVarName -> IO (DepVarName, SelfDistMatrix)
calcSelfDistOneDimDepVar getPos vec varName =
    let names = V.fromList $ getKeys $ getPos (V.head vec)
    in case V.elemIndex varName names of
        Just ix -> do
            let pos  = V.map (anyPosFromDepVarsPos . getPos) vec
            fmap (varName,) (computeArbitrarySelfDistMatrix ix pos)
        Nothing -> error ("Unknown dependent variable: " ++ varName)

calcObsObsDistances :: Double -> V.Vector Observation -> [IndepVarName] -> IO SelfDistMatrixPerIndepVar
calcObsObsDistances scale obs varsToCompute = do
    let indepVarNames = case posFromObs (V.head obs) of
                          IndepSpatTempPos _ -> ["space","time"]
                          IndepArbitraryDimPos (ValuesPerIndepVar ns _) -> V.toList ns
        selected = filter (`elem` varsToCompute) indepVarNames
    mats <- mapM (calcSelfDistOneDim scale (\(Observation _ _ (HyperPos pos _) _) -> pos) obs) selected
    pure (SelfDistMatrixPerIndepVar mats)

calcGridGridDistances :: Double -> V.Vector IndepVarsPos -> [IndepVarName] -> IO SelfDistMatrixPerIndepVar
calcGridGridDistances scale grid varsToCompute = do
    let indepVarNames = case grid V.! 0 of
                        IndepSpatTempPos _ -> ["space","time"]
                        IndepArbitraryDimPos (ValuesPerIndepVar ns _) -> V.toList ns
        selected = filter (`elem` varsToCompute) indepVarNames
    mats <- mapM (calcSelfDistOneDim scale id grid) selected
    pure (SelfDistMatrixPerIndepVar mats)

calcSelfDistOneDim :: Double -> (a -> IndepVarsPos) -> V.Vector a -> IndepVarName -> IO (IndepVarName, SelfDistMatrix)
calcSelfDistOneDim spatScale getPos vec varName =
  case getPos (V.head vec) of
    IndepSpatTempPos _ ->
      case varName of
        "space" -> do
          let pos  = V.map (spatPosFromIndepVarsPos . getPos) vec
          fmap (varName,) (computeSpaceSelfDistMatrix spatScale pos)
        "time"  -> do
          let pos  = V.map (tempPosFromIndepVarsPos . getPos) vec
          fmap (varName,) (computeTimeSelfDistMatrix pos)
        _       -> error ("Unknown ST variable: " ++ varName)
    IndepArbitraryDimPos (ValuesPerIndepVar names _) ->
      case V.elemIndex varName names of
        Just ix -> do
          let pos  = V.map (anyPosFromIndepVarsPos . getPos) vec
          fmap (varName,) (computeArbitrarySelfDistMatrix ix pos)
        Nothing -> error ("Unknown AR variable: " ++ varName)

computeSpaceSelfDistMatrix :: Double -> V.Vector SpatPos -> IO SelfDistMatrix
computeSpaceSelfDistMatrix spatScale vec = do
    let n     = V.length vec
        nHalf = n*(n+1) `div` 2
    mv <- VSM.new nHalf
    forM_ [0..n-1] $ \i ->
        let s1 = vec V.! i
        in forM_ [0..i] $ \j ->
             let s2 = vec V.! j
             in VSM.write mv (idxHalf i j) (spatialDistSpatPos s1 s2 * spatScale)
    frozen <- VS.unsafeFreeze mv
    pure (SelfDistMatrix frozen)

computeTimeSelfDistMatrix :: V.Vector TempPos -> IO SelfDistMatrix
computeTimeSelfDistMatrix vec = do
    let n     = V.length vec
        nHalf = n*(n+1) `div` 2
    mv <- VSM.new nHalf
    forM_ [0..n-1] $ \i ->
        let t1 = vec V.! i
        in forM_ [0..i] $ \j ->
             let t2 = vec V.! j
             in VSM.write mv (idxHalf i j) (temporalDistTempPos t1 t2)
    frozen <- VS.unsafeFreeze mv
    pure (SelfDistMatrix frozen)

computeArbitrarySelfDistMatrix :: Int -> V.Vector (VS.Vector Double) -> IO SelfDistMatrix
computeArbitrarySelfDistMatrix ix vec = do
    let n     = V.length vec
        nHalf = n*(n+1) `div` 2
    mv <- VSM.new nHalf
    forM_ [0..n-1] $ \i ->
        let vs1 = vec V.! i
        in forM_ [0..i] $ \j ->
             let vs2 = vec V.! j
             in VSM.write mv (idxHalf i j) (abs (vs1 VS.! ix - vs2 VS.! ix))
    frozen <- VS.unsafeFreeze mv
    pure (SelfDistMatrix frozen)

selfDistMatrixToFlatHalf :: SelfDistMatrixPerIndepVar -> IndepVarsDistFlat
selfDistMatrixToFlatHalf sdmPerIndepVar =
    let mats   = getValues sdmPerIndepVar
        stride = length mats -- number of dimensions
        nHalf  = case mats of
            (SelfDistMatrix v:_) -> VS.length v
            [] -> throwL "selfDistMatrixToFlatHalf: empty SelfDistMatrixPerIndepVar"
        -- tags vector: same simple heuristic as crossDistMatrixToFlat
        tagsVec = VS.replicate nHalf $
                    case listToMaybe (getKeys sdmPerIndepVar) of
                      Just "space" -> False
                      _            -> True
        -- payload: stride‐interleave each dim's half vector
        payloadVec = VS.generate (nHalf * stride) $ \k ->
            let (idx, dimIx) = k `divMod` stride
                SelfDistMatrix vec = mats !! dimIx
            in vec VS.! idx
    in IndepVarsDistFlat tagsVec payloadVec stride

-- distance helper functions

{-# INLINE temporalDistTempPos #-}
temporalDistTempPos :: TempPos -> TempPos -> Double
temporalDistTempPos (TempPos t1) (TempPos t2) = temporalDistYearBCAD t1 t2

{-# INLINE temporalDistYearBCAD #-}
temporalDistYearBCAD :: YearBCAD -> YearBCAD -> Double
temporalDistYearBCAD t1 t2 = fromIntegral $ abs (t1 - t2)

{-# INLINE spatialDistSpatPos #-}
spatialDistSpatPos :: SpatPos -> SpatPos -> Double
spatialDistSpatPos (SpatPosCartesian p1) (SpatPosCartesian p2) = spatialDistCartesianPos p1 p2
spatialDistSpatPos (SpatPosLongLat p1) (SpatPosLongLat p2) = spatialDistLongLatPos p1 p2
spatialDistSpatPos _ _ = error "Can not be calculated"

{-# INLINE spatialDistCartesianPos #-}
spatialDistCartesianPos :: CartesianPos -> CartesianPos -> Double
spatialDistCartesianPos (CartesianPos _ _ x1 y1) (CartesianPos _ _ x2 y2) =
    sqrt (((x1 - x2) ** 2) + ((y1 - y2) ** 2))

-- Haversine distance in metres
{-# INLINE spatialDistLongLatPos #-}
spatialDistLongLatPos :: LongLatPos -> LongLatPos -> Double
spatialDistLongLatPos (LongLatPos _ _ (Longitude lon1) (Latitude lat1))
                      (LongLatPos _ _ (Longitude lon2) (Latitude lat2)) =
    let r = 6371000  -- radius of Earth in metres
        toRadians n = n * pi / 180
        square x = x * x
        cosr = cos . toRadians
        dlat = toRadians (lat1 - lat2) / 2
        dlon = toRadians (lon1 - lon2) / 2
        a = square (sin dlat) + cosr lat1 * cosr lat2 * square (sin dlon)
        c = 2 * atan2 (sqrt a) (sqrt (1 - a))
    in r * c

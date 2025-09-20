{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LocEst.CLI.Search where

import LocEst.Types
import           LocEst.Parsers
--import           LocEst.CoreAlgorithms
import LocEst.Exceptions (throwL)
import LocEst.MathUtils
import           LocEst.Distance

import qualified Data.Vector       as V
import qualified Data.Vector.Mutable   as VM
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable           as VUM
import           System.IO                     (hPutStrLn, stderr)
import           Control.Monad                 (replicateM, zipWithM_)
import           Statistics.Distribution           (logDensity, quantile)
import           Statistics.Distribution.StudentT  (StudentT)
import           Statistics.Distribution.Transform (LinearTransform)
import qualified Numeric.LinearAlgebra             as M
import qualified Data.Vector.Storable              as VS
import           Data.Conduit                  ((.|))
import qualified Data.Conduit                  as Con
import qualified Data.Conduit.Algorithms.Async as ConAA
import qualified Data.Conduit.Combinators      as ConC
import qualified Data.Conduit.List             as ConL
import qualified Data.Csv as Csv
import GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as Bchs
import Conduit (liftIO)

data SearchOptions = SearchOptions
    { _searchInObservationFile   :: FilePath
    , _searchInIndepPredGridFile :: FilePath
    , _searchInTempGrid          :: Maybe [AbsRelTempPos]
    , _searchInDepSearchGrid     :: Maybe DepVarsPredGridSettings
    , _searchAlgorithm           :: KernelDefinition
    , _searchOutFile             :: Maybe FilePath
    }

data DepVarsPredGridSettings =
      DirectDepVarsGridSettings [DepVarsPos]
    | SearchObsDepVarsGridSettings FilePath

runSearch :: SearchOptions -> Double -> IO ()
runSearch (SearchOptions
    inObsFile inIndepVarsPredGridFile maybeTempGrid inMaybeDepSearchGrid kernelDefinition outFile
    ) spatDistUnitScaling = do
    -- list of variables
    let depVars   = getKeys kernelDefinition
        indepVars = getKeys $ _kodvLengths $ head $ _kdefPerDepVar kernelDefinition
        kernels   = getValues kernelDefinition
    -- read observations
    !obs <- filterVarsInObs depVars indepVars <$> readObservations inObsFile
    -- read indepVar prediction grid positions
    !indepPredGrid <- V.map (filterVarsInIndepVarsPos indepVars) <$> readIndepVarsPos inIndepVarsPredGridFile
    -- read depVar search grid
    !depSearchGrid <- traverse (readDepVarsPredGrid depVars indepVars) inMaybeDepSearchGrid
    -- permutations
    hPutStrLn stderr "Preparing permutations"
    let permutations = createPermutations2 obs Nothing
    -- run interpolation and search
    Con.runConduitRes $
           ConC.yieldMany permutations
        .| ConL.concatMapM (liftIO . core spatDistUnitScaling depVars kernels indepPredGrid depSearchGrid)
        -- .| progress 1000 (Just numPerms)
        -- .| normalise normalisation
        .| sinkNamedCSV outFile
    
    --let interpolPerDepVar = zipWith3 (interpol obs dists) depVars kernels (repeat Nothing)
    
    --putStrLn $ show interpolPerDepVar
    
    putStrLn "Done"

core :: Double -> [DepVarName] -> [KernelOneDepVar] -> V.Vector IndepVarsPos -> Maybe (V.Vector DepVarsPredPos) -> Permutation2 -> IO [SearchResultRow]
core spatDistUnitScaling depVars kernelsPerDepVar grid searchDepVarPos perm@(Permutation2 tempSamplingIteration obs) = do
    dists <- calcObsGridDistances spatDistUnitScaling obs grid
    -- TODO: case maybeDistFile of ...
    -- ... 
    let perDepVar = zipWith (interpol obs dists searchDepVarPos) depVars kernelsPerDepVar
    let n = V.length grid
    let rowsForGridIdx :: Int -> [SearchResultRow]
        rowsForGridIdx i =
            -- for each depvar, explode KAS2 at grid index i into KAS3 rows
            concatMap (\vkas2 ->
              let kas2 = vkas2 V.! i
                  kas3s = explodeKAS2 kas2
                  gp    = grid V.! i
              in [ SearchResultRow
                     { _srrTempSamplingIteration = tempSamplingIteration
                     , _srrGrid                  = gp
                     , _srrInterpolation         = kas3
                     }
                 | kas3 <- kas3s
                 ]
            ) perDepVar

    return $ concatMap rowsForGridIdx [0 .. n-1]
         
data SearchResultRow = SearchResultRow {
      _srrTempSamplingIteration :: Int
    , _srrGrid                  :: IndepVarsPos
    , _srrInterpolation         :: InterpolationResultOneDepVar3
} deriving (Show, Generic)

instance Csv.DefaultOrdered SearchResultRow where
  headerOrder (SearchResultRow _ grid kas3) =
      Csv.header ["temp_sampling_iteration"]
   <> Csv.headerOrder grid
   <> Csv.headerOrder kas3

instance Csv.ToRecord SearchResultRow where
  toRecord (SearchResultRow tsi grid kas3) =
      Csv.record [Csv.toField tsi]
   <> Csv.toRecord grid
   <> Csv.toRecord kas3

data InterpolationResultOneDepVar3 = KAS3 {
          _irKAS3DepVarName       :: DepVarName   -- name of the dependent variable
        , _irKAS3EffN             :: Double       -- effective number of samples
        , _irKAS3WeightedVar      :: Double       -- weighted variance
        , _irKAS3WeightedVarPrior :: Double       -- weighted variance with prior
        , _irKAS3Posterior        :: Bool      -- could a posterior distribution be calculated?
        , _irKAS3LowerBound       :: Double    -- lower boundary of the 95% interval
        , _irKAS3Median           :: Double       -- median (weighted average)
        , _irKAS3UpperBound       :: Double    -- upper boundary of the 95% interval
        , _irKAS3SearchPos        :: Maybe DepVarsPredPos
        , _irKAS3LogLikelihood    :: Maybe Double -- Log-likelihood for search value
    } deriving (Eq, Show, Generic)

explodeKAS2 :: InterpolationResultOneDepVar2 -> [InterpolationResultOneDepVar3]
explodeKAS2 (KAS2 dep neff v vp po lb m ub mSearch mLL) =
  let n = maybe 0 V.length mSearch
      mk i = KAS3
               { _irKAS3DepVarName       = dep
               , _irKAS3EffN             = neff
               , _irKAS3WeightedVar      = v
               , _irKAS3WeightedVarPrior = vp
               , _irKAS3Posterior        = po
               , _irKAS3LowerBound       = lb
               , _irKAS3Median           = m
               , _irKAS3UpperBound       = ub
               , _irKAS3SearchPos        = fmap (V.! i) mSearch
               , _irKAS3LogLikelihood    = fmap (V.! i) mLL
               }
  in map mk [0..n-1]

instance Csv.DefaultOrdered InterpolationResultOneDepVar3 where
  headerOrder (KAS3 _ _ _ _ _ _ _ _ mSearch mLL) =
      let base = Csv.header ["depVar","interpol_neff","interpol_var","interpol_var_prior"
                            ,"interpol_post","interpol_low","interpol_median","interpol_up"]
          searchHdr = maybe V.empty Csv.headerOrder mSearch
          llHdr     = if mLL == Nothing then V.empty else Csv.header ["log_likelihood"]
      in base <> searchHdr <> llHdr

instance Csv.ToRecord InterpolationResultOneDepVar3 where
  toRecord (KAS3 dep neff v vp po lb m ub mSearch mLL) =
      Csv.record [Csv.toField dep]
   <> Csv.record [ Csv.toField neff
                 , Csv.toField v
                 , Csv.toField vp
                 , Csv.toField (OutBool po)
                 , Csv.toField (OutDouble lb)
                 , Csv.toField m
                 , Csv.toField (OutDouble ub)
                 ]
   <> maybe V.empty Csv.toRecord mSearch
   <> maybe V.empty (V.singleton . Csv.toField . OutDouble) mLL


zipWithN :: ([a] -> b) -> [V.Vector a] -> V.Vector b
zipWithN f vs 
  | null vs   = V.empty
  | otherwise = V.generate (V.length $ head vs) (\i -> f (map (V.! i) vs))

data Permutation2 = Permutation2 {
      _permTempSamplingIteration :: Int
    , _permObs                   :: V.Vector Observation
} deriving (Show)

createPermutations2 :: V.Vector Observation -> Maybe TempSampleMatrix -> [Permutation2]
createPermutations2 obs maybeTempSampleMatrix = do
    -- apply temp resampling to obs
    tempSamp <- [0..(nrTempSamples maybeTempSampleMatrix - 1)]
    let modObs = V.map (applyTempSamp maybeTempSampleMatrix tempSamp) obs
    return $ Permutation2 tempSamp modObs

nrTempSamples :: Maybe TempSampleMatrix -> Int
nrTempSamples Nothing                         = 1
nrTempSamples (Just (TempSampleMatrix n _ _)) = n

applyAbsRelTempPos :: YearBCAD -> IndepVarsPos -> IndepVarsPos
applyAbsRelTempPos _ _ = undefined

applyTempSamp :: Maybe TempSampleMatrix -> Int -> Observation -> Observation
applyTempSamp (Just m) i obs@(Observation i1 i2 (HyperPos (IndepSpatTempPos (SpatTempPos i3 (TempPos age))) i4) i5) =
    let obsIndex = getIndex obs
        newage = lookUpTempSample m i obsIndex
    in Observation i1 i2 (HyperPos (IndepSpatTempPos (SpatTempPos i3 (TempPos newage))) i4) i5
applyTempSamp _ _ obs = obs

readDepVarsPredGrid :: [String] -> [String] -> DepVarsPredGridSettings -> IO (V.Vector DepVarsPredPos)
readDepVarsPredGrid depVars _ (DirectDepVarsGridSettings depVarsPos) = do
    let depVarsPosReordered = V.map (filterByKey depVars) $ V.fromList depVarsPos
    return $ V.map DepVarsPredPosDirect depVarsPosReordered
readDepVarsPredGrid depVars indepVars (SearchObsDepVarsGridSettings path) = do
    !obs <- readObservations path -- search observations
    let obsFiltered = filterVarsInObs depVars indepVars obs
    return $ V.map DepVarsPredPosSearchObs obsFiltered

interpol :: V.Vector Observation -> V.Vector IndepVarsDist -> Maybe (V.Vector DepVarsPredPos)
         -> DepVarName -> KernelOneDepVar 
         -> V.Vector InterpolationResultOneDepVar2
interpol obs dists maybeSearchValues depVar kernel =
    let values  = VS.convert $ V.map (getDepVarsPos depVar) obs
        weights = M.reshape (V.length obs) $ VS.convert $ V.map (getWeight2 kernel) dists
        searchValues = fmap (V.map (getDepVarsPos2 depVar)) maybeSearchValues
    in V.map (search searchValues) $ kas weights values
    where
        search searchValues (neff, wvb, wv, mu, Right distribution) =
            let lower  = quantile distribution 0.025
                median = mu -- quantile distribution 0.5
                upper  = quantile distribution 0.975
                logL   = fmap (V.map $ logDensity distribution) searchValues -- log-likelihood
            in KAS2 depVar neff wvb wv True lower median upper maybeSearchValues logL
        search searchValues (neff, wvb, wv, mu, Left _) = case searchValues of
            Just x  -> KAS2 depVar neff wvb wv False (-inf) mu inf maybeSearchValues (Just (V.replicate (V.length x) (-inf)))
            Nothing -> KAS2 depVar neff wvb wv False (-inf) mu inf maybeSearchValues Nothing

-- | A data type for interpolation output for one dependent variable
data InterpolationResultOneDepVar2 = KAS2 {
          _irKASDepVarName       :: DepVarName   -- name of the dependent variable
        , _irKASEffN             :: Double       -- effective number of samples
        , _irKASWeightedVar      :: Double       -- weighted variance
        , _irKASWeightedVarPrior :: Double       -- weighted variance with prior
        , _irKASPosterior        :: Bool      -- could a posterior distribution be calculated?
        , _irKASLowerBound       :: Double    -- lower boundary of the 95% interval
        , _irKASMedian           :: Double       -- median (weighted average)
        , _irKASUpperBound       :: Double    -- upper boundary of the 95% interval
        , _irKASSearchPos        :: Maybe (V.Vector DepVarsPredPos) -- search values
        , _irKASLogLikelihood    :: Maybe (V.Vector Double) -- Log-likelihood for search value
    } deriving (Eq, Show, Generic)

sumRows :: M.Matrix M.R -> M.Vector M.R
sumRows m = M.flatten $ m M.<> M.konst 1 (M.cols m, 1)

kas :: M.Matrix M.R -> M.Vector M.R -> V.Vector (Double, Double, Double, Double, Either String (LinearTransform StudentT))
kas weights y =
    V.zipWith6 (\neff wvb wv _mu _scale _dof -> (neff, wvb, wv, _mu, generalizedStudentT _mu _scale _dof))
        (V.convert totalWeight) (V.convert weightedVarBasic) (V.convert weightedVar)
        (V.convert mu) (V.convert scale) (V.convert dof)
    where
      totalWeight = sumRows weights
      weightedAvg = M.flatten (weights M.<> M.asColumn y) / totalWeight
      values = M.fromRows $ replicate (M.rows weights) y
      weightedVarBasic = sumRows (weights * (values - M.asColumn weightedAvg) ** 2) / (totalWeight - 1)
      meanY = M.sumElements y / fromIntegral (M.size y)
      varSample = M.dot (y - M.scalar meanY) (y - M.scalar meanY) / fromIntegral (M.size y - 1)
      scaledS2 = (totalWeight - 1) * weightedVarBasic
      weightedVar = (scaledS2 + M.scalar varSample) / (totalWeight + 1)
      mu = weightedAvg
      scale = M.cmap sqrt ((1 + 1/(totalWeight + 1)) * weightedVar)
      dof = totalWeight

computeWeight :: KernelShape -> SquaredWeightedDist -> Double
computeWeight SquaredExponential d = 1 / exp d
computeWeight Linear             d = 1 / (1 + sqrt d)

getWeight2 :: KernelOneDepVar -> IndepVarsDist -> Double
getWeight2 (KernelOneDepVar _ shape lengths) dists =
    computeWeight shape (squaredWeightedDist lengths dists)
    where
        squaredWeightedDist :: KernelLengths -> IndepVarsDist -> Double
        squaredWeightedDist
            (KernelLengths (ValuesPerIndepVar [(_,spaceKernelWidth), (_,timeKernelWidth)]))
            (IndepSpatTempDist (SpatTempDist spatDist tempDist)) =
            (spatDist / spaceKernelWidth) ** 2 + (tempDist / timeKernelWidth) ** 2
        squaredWeightedDist
            kernLengths
            (IndepArbitraryDimDist namedDists) =
            let distances = getValues namedDists
                thetas    = getValues kernLengths
            in foldSum (zipWith (\d t -> (d / t) ** 2) distances thetas)
        squaredWeightedDist _ _ =
            throwL "mismatch of independent variable definitions in weight calculation"

makeObsGridPairs :: V.Vector Observation -> V.Vector IndepVarsPos -> [(Int, (Observation, IndepVarsPos))]
makeObsGridPairs obs grid =
    let obsIndexMax = V.length obs - 1
        gridIndexMax = V.length grid - 1
        obsGridPairs = [(obs V.! x, grid V.! y) | y <- [0..gridIndexMax], x <- [0..obsIndexMax]]
    in zip [0..] obsGridPairs

calcObsGridDistances :: Double -> V.Vector Observation -> V.Vector IndepVarsPos -> IO (V.Vector IndepVarsDist)
calcObsGridDistances spatDistUnitScaling obs grid = do
    let nrObs = V.length obs
        nrGrid = V.length grid
        nrPairs = nrObs * nrGrid
        obsGridPairs = makeObsGridPairs obs grid
        (Observation _ _ (HyperPos indepPos _) _) = V.head obs
    weightVec <- VM.new nrPairs
    mapM_ (computeDist weightVec) obsGridPairs
    weightVecNonMut <- V.unsafeFreeze weightVec
    return weightVecNonMut --AUDistMatrix nrObs nrGrid weightVecNonMut
    where
        computeDist :: VM.IOVector IndepVarsDist -> (Int, (Observation, IndepVarsPos)) -> IO ()
        computeDist weightVec (i, (Observation i1 _ (HyperPos p1 _) _, p2)) = do
            let dist = getDist2 spatDistUnitScaling p1 p2
            VM.write weightVec i dist

getDist2 :: Double -> IndepVarsPos -> IndepVarsPos -> IndepVarsDist
-- spatiotemporal distances
getDist2 spatDistUnitScaling
        (IndepSpatTempPos (SpatTempPos spatPos1 tempPos1))
        (IndepSpatTempPos (SpatTempPos spatPos2 tempPos2)) =
        let spatDist = spatialDistSpatPos spatPos1 spatPos2
            spaceDistScaled = spatDist * spatDistUnitScaling
            tempDist = temporalDistTempPos tempPos1 tempPos2
        in IndepSpatTempDist (SpatTempDist spaceDistScaled tempDist)
-- arbitrary dim distances
getDist2 spatDistUnitScaling
        (IndepArbitraryDimPos arbitraryDimPos1)
        (IndepArbitraryDimPos arbitraryDimPos2) =
        let keys = getKeys arbitraryDimPos1
            obsPos  = getValues arbitraryDimPos1
            gridPos = getValues arbitraryDimPos2
            arbitraryDimDist = makeValuesPerIndepVar $ zip keys (allDistances obsPos gridPos)
        in IndepArbitraryDimDist arbitraryDimDist
-- wrong input
getDist2 _ _ _ = throwL "mismatch of independent variable definitions in distance calculation"

makeObsPairs :: V.Vector Observation -> [(Int, (Observation, Observation))]
makeObsPairs obs =
    let obsIndexMax = V.length obs - 1
        obsPairs = [(obs V.! x, obs V.! y) | x <- [0..obsIndexMax], y <- [0..obsIndexMax], x > y]
    in zip [0..] obsPairs

calcObsDistances :: Double -> V.Vector Observation -> IO MatrixPerIndepVar
calcObsDistances spatDistUnitScaling obs = do
    let obsPairs = makeObsPairs obs
        nrPairs = length obsPairs
        (Observation _ _ (HyperPos indepPos _) _) = V.head obs
    case indepPos of
        -- spatiotemporal system
        (IndepSpatTempPos _) -> do
            -- create mutable vectors to write distances directly
            spaceVec <- VUM.new nrPairs
            timeVec  <- VUM.new nrPairs
            -- calculate and write distances to mutable memory
            mapM_ (distSpaceTime spaceVec timeVec) obsPairs
            -- make result vectors immutable for easier handling
            spaceVecNonMut <- VU.unsafeFreeze spaceVec
            timeVecNonMut  <- VU.unsafeFreeze timeVec
            return $ MatrixPerIndepVar [("space", SUDistMatrix spaceVecNonMut), ("time", SUDistMatrix timeVecNonMut)]
        -- arbitrary dimension system
        (IndepArbitraryDimPos pos@(ValuesPerIndepVar l)) -> do
            arbitraryVecs <- replicateM (length l) (VUM.new nrPairs)
            mapM_ (distArbitrary arbitraryVecs) obsPairs
            arbitraryVecsNonMut <- mapM VU.unsafeFreeze arbitraryVecs
            return $ MatrixPerIndepVar $ zipWith (\name vec -> (name, SUDistMatrix vec)) (getKeys pos) arbitraryVecsNonMut
    where
        distSpaceTime :: VUM.IOVector Double -> VUM.IOVector Double -> (Int, (Observation, Observation)) -> IO ()
        distSpaceTime
            spaceVec timeVec
            (i,
            (Observation i1 _ (HyperPos (IndepSpatTempPos (SpatTempPos s1 t1)) _) _,
             Observation i2 _ (HyperPos (IndepSpatTempPos (SpatTempPos s2 t2)) _) _)
            ) = do
            let timeDist  = temporalDistTempPos t1 t2
                spaceDist = spatialDistSpatPos s1 s2
                spaceDistScaled = spaceDist * spatDistUnitScaling
            -- write distances to mutable vector
            VUM.write spaceVec i spaceDistScaled
            VUM.write timeVec  i timeDist
        distSpaceTime _ _ _ = error "impossible state in spatial independent variable distance calculation"
        distArbitrary :: [VUM.IOVector Double] -> (Int, (Observation, Observation)) -> IO ()
        distArbitrary
            arbitraryVecs
            (i,
            (Observation _ _ (HyperPos (IndepArbitraryDimPos p1) _) _,
             Observation _ _ (HyperPos (IndepArbitraryDimPos p2) _) _)
            ) = do
            -- this assumes that p1 and p2 have the same order of indep variables
            let arbitraryDists = allDistances (getValues p1) (getValues p2)
            zipWithM_ (`VUM.write` i) arbitraryVecs arbitraryDists
        distArbitrary _ _ = error "impossible state in arbitrary independent variable distance calculation"

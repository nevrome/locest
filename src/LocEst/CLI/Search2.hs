{-# LANGUAGE BangPatterns        #-}

module LocEst.CLI.Search2 where

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

data Search2Options = Search2Options
    { _search2InObservationFile   :: FilePath
    , _search2InIndepPredGridFile :: FilePath
    , _search2InDepSearchGrid     :: Maybe DepVarsPredGridSettings
    , _search2Algorithm           :: KernelDefinition
    , _searchOutFile              :: Maybe FilePath
    }

data DepVarsPredGridSettings =
      DirectDepVarsGridSettings [DepVarsPos]
    | SearchObsDepVarsGridSettings FilePath

runSearch2 :: Search2Options -> Double -> IO ()
runSearch2 (Search2Options
    inObsFile inIndepVarsPredGridFile inMaybeDepSearchGrid kernelDefinition outFile
    ) spatDistUnitScaling = do
    -- list of variables
    let depVars   = getKeys kernelDefinition
        indepVars = getKeys $ _kodvLengths $ head $ _kdefPerDepVar kernelDefinition
        kernels   = getValues kernelDefinition
    -- read observations
    !obs <- filterVarsInObs depVars indepVars <$> readObservations inObsFile
    -- read indepVar prediction grid positions
    !indepPredGrid <- V.map (filterVarsInIndepVarsPos indepVars) <$> readIndepVarsPos inIndepVarsPredGridFile
    dists <- calcObsGridDistances spatDistUnitScaling obs indepPredGrid
    -- TODO: case maybeDistFile of ...
    -- ... 
    -- read depVar search grid
    !depSearchGrid <- traverse (readDepVarsPredGrid depVars indepVars) inMaybeDepSearchGrid
    -- run interpolation and search
    --Con.runConduitRes $
        
        
    
    
    let interpolPerDepVar = zipWith3 (interpol obs dists) depVars kernels (repeat Nothing)
    
    putStrLn $ show interpolPerDepVar
    
    putStrLn "Done"


readDepVarsPredGrid :: [String] -> [String] -> DepVarsPredGridSettings -> IO (V.Vector DepVarsPredPos)
readDepVarsPredGrid depVars _ (DirectDepVarsGridSettings depVarsPos) = do
    let depVarsPosReordered = V.map (filterByKey depVars) $ V.fromList depVarsPos
    return $ V.map DepVarsPredPosDirect depVarsPosReordered
readDepVarsPredGrid depVars indepVars (SearchObsDepVarsGridSettings path) = do
    !obs <- readObservations path -- search observations
    let obsFiltered = filterVarsInObs depVars indepVars obs
    return $ V.map DepVarsPredPosSearchObs obsFiltered

interpol :: V.Vector Observation -> V.Vector IndepVarsDist
         -> DepVarName -> KernelOneDepVar -> Maybe Double
         -> V.Vector InterpolationResultOneDepVar
interpol obs dists depVar kernel maybeSearchValue =
    let values  = VS.convert $ V.map (getDepVarsPos depVar) obs
        weights = M.reshape (V.length obs) $ VS.convert $ V.map (getWeight2 kernel) dists
    in V.map search $ kas weights values
    where
        search (neff, wvb, wv, mu, Right distribution) =
            let lower  = quantile distribution 0.025
                median = mu -- quantile distribution 0.5
                upper  = quantile distribution 0.975
                logL   = fmap (logDensity distribution) maybeSearchValue -- log-likelihood
            in KAS depVar neff wvb wv True lower median upper logL
        search (neff, wvb, wv, mu, Left _) = case maybeSearchValue of
            Just _  -> KAS depVar neff wvb wv False (-inf) mu inf (Just (-inf))
            Nothing -> KAS depVar neff wvb wv False (-inf) mu inf Nothing

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

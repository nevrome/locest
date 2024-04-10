{-# LANGUAGE BangPatterns        #-}

module LocEst.CLI.Vario where

import LocEst.Parsers
import LocEst.Types
import LocEst.Distance
import LocEst.MathUtils

import           System.IO       (hPutStrLn, stderr)
import Data.List (tails, foldl')
import qualified Data.Vector.Unboxed as VU

data VarioOptions = VarioOptions {
    _voInObservationFile :: FilePath,
    _voVariogramOutFile  :: Maybe FilePath
}

runVario :: VarioOptions -> IO ()
runVario (VarioOptions inObsFile _) = do
    -- read observations
    hPutStrLn stderr "Reading observations"
    !observationsUnindexed <- readObservations inObsFile
    let observations = zipWith setIndex observationsUnindexed [0..]
    -- calculate pairwise distances
    hPutStrLn stderr "Calculating pairwise distances for all independent variables"
    let distsPerIndepVar = calcPairwiseDistances observations
    hPutStrLn stderr "Calculating pairwise distances for all dependent variables"
    --let distsPerDepVar   = calcPairwiseDistances observations
    -- continue per independent variable
    -- huhu
    hPutStrLn stderr "wip"

binAndFitPerIndepVar :: IndepVarName -> SUDistMatrix -> IO ()
binAndFitPerIndepVar indepVarName (SUDistMatrix _ _ distVec) = do
    hPutStrLn stderr $ "Working on: " ++ indepVarName
    -- prepare bins
    let minValue = VU.minimum distVec
        maxValue = VU.maximum distVec
        stepWidth = (maxValue - minValue)/20
        stepsSingle = [minValue,minValue+stepWidth..maxValue]
        steps = zip (init stepsSingle) (tail stepsSingle)
    -- realize bins and calculate semi-variance per bin
        --hu = VA.sort distVec

    hPutStrLn stderr "wip"

calcMatheron :: [Double] -> Double
calcMatheron xs = (1 / (2 * n)) * foldl' (\acc x -> acc + ((x - mean)^2)) 0 xs
    where
        n = fromIntegral $ length xs
        mean = foldSum xs / n

calcIndepVarPairwiseDistances :: [Observation] -> [(IndepVarName, SUDistMatrix)]
calcIndepVarPairwiseDistances obs = reshape [obsobsDist x y | y <- obs, (x:_) <- tails obs]
    where
        dist :: Observation -> Observation -> [(IndepVarName, Double)]
        dist
            (Observation _ _ (HyperPos (IndepSpatTempPos p1) _))
            (Observation _ _ (HyperPos (IndepSpatTempPos p2) _)) =
            [("space", spatialDistSpatTempPos p1 p2), ("time", temporalDistSpatTempPos p1 p2)]
        dist
            (Observation _ _ (HyperPos (IndepArbitraryDimPos p1) _))
            (Observation _ _ (HyperPos (IndepArbitraryDimPos p2) _)) =
            -- this assumes that p1 and p2 have the same order of indep variables
            zip (getKeys p1) (allDistances (getValues p1) (getValues p2))
        dist _ _ = error "Impossible state in indep distance calculation"

reshape :: [[(String, Double)]] -> [(String, SUDistMatrix)]
reshape = map (\x -> (fst $ head x, SUDistMatrix $ VU.fromList $ map snd x))

{-# LANGUAGE ScopedTypeVariables   #-}

module LocEst.CLI.Search where

import LocEst.Types
import LocEst.Parsers
import LocEst.Distance
import LocEst.Math
import Data.List (zip5)

data SearchOptions = SearchOptions
    { _searchInObservationFile :: FilePath
    , _searchInSearchPosFile   :: FilePath
    , _searchOutFile           :: FilePath
    }

runSearch :: SearchOptions -> IO ()
runSearch (
    SearchOptions inObsFile inSearchPosFile outFile
    ) = do
    
    allObservations <- readSpatTempObs inObsFile
    pipeSpatTempPosConduit inSearchPosFile outFile (myFunc allObservations)


myFunc :: [SpatTempObs] -> SpatTempPos -> SpatTempProb
myFunc allSpatTempObs spatTempPosRaw =
    let spatTempPos = spatTempPosRaw {_temporalPos = SimpleYearBCAD (-6000) }
        allSpatDists   = map (spatialDistSpatTempPos spatTempPos . _stpoSpatTempPos) allSpatTempObs
        allSpatDistsKM = map (/ 1000) allSpatDists
        allTempDists   = map (temporalDistSpatTempPos spatTempPos . _stpoSpatTempPos) allSpatTempObs
        allPCMeans     = map _stpopc1 allSpatTempObs
        allPCSDs       = map (\(s,t) -> 0.0001 * s + 0.0001 * t) (zip allSpatDistsKM allTempDists)
        allDensities   = map (\(mean,sd) -> dnorm mean sd 0.0461299) (zip allPCMeans allPCSDs)
        minPC1         = minimum allPCMeans
        maxPC1         = maximum allPCMeans
        allIntegrals   = map (\(mean,sd) -> integration (dnorm mean sd) minPC1 maxPC1) (zip allPCMeans allPCSDs)
        meanDens       = 
            -- avg allDensities -- too smooth, low densities pull the mean down
            maximum allDensities -- too aggressive?



    in --error $ show $ zip5 allSpatDistsKM allTempDists allPCMeans allPCSDs allDensities
       SpatTempProb { _stprspatTempPos = spatTempPos, _stprprobability = meanDens }

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = 
    h / 2 * (f a + f b + 2 * partial_sum)
    where 
        h = (b - a) / 1000 
        most_parts  = map f (pointsWithOffset (1000-1) h a)  
        partial_sum = sum most_parts

pointsWithOffset :: Double -> Double -> Double -> [Double]
pointsWithOffset x1 x2 offset = map (+offset) (points x1 x2)

points  :: Double -> Double -> [Double]
points x1 x2 
    | x1 <= 0 = []
    | otherwise = (x1*x2) : points (x1-1) x2
{-# LANGUAGE OverloadedStrings #-}

module LocEst.CLI.Grid where

import LocEst.Types
import LocEst.Parsers
import LocEst.Utils

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.List (sort)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import           System.IO                (hPutStrLn, stderr)
import qualified Data.Conduit             as Con
import qualified Data.Conduit.Combinators as ConC
import           Data.Conduit             ((.|))

data GridOptions = GridOptions
    { _gridInPolygonFile :: FilePath
    , _gridResolutionX   :: Double
    , _gridResolutionY   :: Double
    , _gridOutFile       :: Maybe FilePath
    }

runGrid :: GridOptions -> IO ()
runGrid (GridOptions inPolygonFile resolutionX resolutionY outFile) =  do
    polys <- readPolygons inPolygonFile
    let (xmin,ymin,_,ymax) = bbox polys
        ys = [ymin, ymin+resolutionY .. ymax]
        gripPoints = [ p
                     | y <- ys
                     , poly <- polys
                     , interval <- fillPolygonScanline y poly
                     , p <- emitPoints resolutionX xmin [interval] y
                     ]
        indepVarsPos = map (\(x,y) -> IndepSpatTempPos (SpatTempPos (SpatPosCartesian (CartesianPos 0 Nothing x y)) (TempPos 0))) gripPoints
    Con.runConduitRes $
           ConC.yieldMany indepVarsPos
        .| sinkNamedCSV outFile
    hPutStrLn stderr "Done"

-- basic types
type Point   = (Double, Double)
type Ring    = [Point]
type Polygon = [Ring] -- exterior:holes

-- GeoJSON parsing
readPolygons :: FilePath -> IO [Polygon]
readPolygons fp = do
  bs <- BL.readFile fp
  case eitherDecode bs of
    Left err -> error err
    Right gj -> pure (extractPolygons gj)

extractPolygons :: Value -> [Polygon]
extractPolygons (Object o) =
  case o .! "type" of
    Just (String "FeatureCollection") ->
      case o .! "features" of
        Just (Array fs) -> concatMap extractPolygons (V.toList fs)
        _ -> []
    Just (String "Feature") ->
      case o .! "geometry" of
        Just g  -> extractPolygons g
        Nothing -> []
    Just (String "Polygon") ->
      case o .! "coordinates" of
        Just (Array rings) ->
          [ map parseRing (V.toList rings) ]
        _ -> []
    Just (String "MultiPolygon") ->
      case o .! "coordinates" of
        Just (Array polys) ->
          [ map parseRing (V.toList rings)
          | Array rings <- V.toList polys
          ]
        _ -> []
    _ -> []
extractPolygons _ = []

(.!) :: KM.KeyMap Value -> String -> Maybe Value
(.!) o k = KM.lookup (K.fromString k) o

parseRing :: Value -> Ring
parseRing (Array pts) = map parsePoint (V.toList pts)
parseRing _ = []

parsePoint :: Value -> Point
parsePoint (Array v)
    | V.length v >= 2 = case (v V.! 0, v V.! 1) of
        (Number x, Number y) -> (realToFrac x, realToFrac y)
        _ -> error "Invalid coordinate"
parsePoint _ = error "Invalid point"

-- bounding box
bbox :: [Polygon] -> (Double, Double, Double, Double)
bbox polys = foldl' step (inf, inf, -inf, -inf) allPts
  where
    allPts = concatMap concat polys
    step (xmin,ymin,xmax,ymax) (x,y) =
      ( min xmin x
      , min ymin y
      , max xmax x
      , max ymax y
      )

-- polygon fill for a single scanline
fillPolygonScanline :: Double -> Polygon -> [(Double,Double)]
fillPolygonScanline y (outer:holes) =
  subtractHoles outerIntervals holeIntervals
  where
    outerIntervals = toIntervals (scanlineIntersections y outer)
    holeIntervals  = concatMap (toIntervals . scanlineIntersections y) holes
fillPolygonScanline _ _ = []

subtractHoles :: [(Double,Double)] -> [(Double,Double)] -> [(Double,Double)]
subtractHoles exts holes = foldl' subtractOne exts holes

subtractOne :: [(Double,Double)] -> (Double,Double) -> [(Double,Double)]
subtractOne [] _ = []
subtractOne ((a,b):xs) (h1,h2)
  | h2 <= a || h1 >= b = (a,b) : subtractOne xs (h1,h2)
  | h1 <= a && h2 >= b = subtractOne xs (h1,h2)
  | h1 <= a            = (h2,b) : xs
  | h2 >= b            = (a,h1) : xs
  | otherwise          = (a,h1) : (h2,b) : xs

toIntervals :: [Double] -> [(Double,Double)]
toIntervals xs =
  pairUp (sort xs)
  where
    pairUp (a:b:rest) = (a,b) : pairUp rest
    pairUp _          = []

-- scanline intersection: intersect a horizontal line y with a ring
scanlineIntersections :: Double -> Ring -> [Double]
scanlineIntersections y ring =
  [ x
  | ((x1,y1),(x2,y2)) <- edges
  , intersects y1 y2
  , let x = x1 + (y - y1) * (x2 - x1) / (y2 - y1)
  ]
  where
    edges = zip ring (drop 1 (cycle ring))
    intersects y1 y2 = (y1 > y) /= (y2 > y) -- standard half-open rule

-- emit grid points from x-intervals
emitPoints :: Double -> Double -> [(Double,Double)] -> Double -> [Point]
emitPoints dx x0 intervals y =
  [ (x,y)
  | (a,b) <- intervals
  , let n0 = ceiling ((a - x0) / dx) :: Integer
  , let n1 = floor   ((b - x0) / dx) :: Integer
  , n <- [n0 .. n1]
  , let x = x0 + fromIntegral n * dx
  ]

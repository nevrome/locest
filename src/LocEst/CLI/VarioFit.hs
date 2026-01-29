module LocEst.CLI.VarioFit where

import           LocEst.Types
import           LocEst.Parsers
import           LocEst.Utils

import           Data.List (groupBy, sortOn)
import           Data.Function (on)
import           Conduit                      ((.|))
import qualified Data.Conduit                 as Con
import qualified Data.Conduit.Combinators     as ConC
import qualified Numeric.GSL.Minimization as GSL

data VarioFitOptions = VarioFitOptions
    { _vfInFile   :: FilePath
    , _vfOutFile  :: Maybe FilePath
    --, _vfKernels  :: [KernelShape]
    } deriving Show

runVarioFit :: VarioFitOptions -> IO ()
runVarioFit opts = do
    bins <- readEmpiricalVariogram (_vfInFile opts)
    let grouped = groupBins bins
        fits    = concatMap (fitAllKernels [SquaredExponential, Exponential, Linear]) grouped
    Con.runConduitRes $ ConC.yieldMany fits .| sinkNamedCSV (_vfOutFile opts)

groupBins
    :: [EmpiricalVariogramSingleBin]
    -> [ (IndepVarName, DepVarName, [EmpiricalVariogramSingleBin]) ]
groupBins bins =
    let sorted = sortOn (\b -> (_evIndepVar b, _evDepVar b)) bins
        groups = groupBy ((==) `on` key) sorted
    in map mkGroup groups
    where
    key b = (_evIndepVar b, _evDepVar b)
    mkGroup xs =
      let b = head xs
      in (_evIndepVar b, _evDepVar b, xs)

type VariogramModel = Double -> Double -> Double -> Double -> Double
-- γ(h) = nugget + sill * f(h / range)

variogramModel :: KernelShape -> VariogramModel
variogramModel SquaredExponential =
    \nug sill range h -> nug + sill * (1 - exp (- ((h * h) / (range * range))))
variogramModel Exponential =
    \nug sill range h -> nug + sill * (1 - exp (- (h / range)))
variogramModel Linear =
    \nug sill range h -> nug + sill * min 1 (h / range)

fitAllKernels
    :: [KernelShape]
    -> (IndepVarName, DepVarName, [EmpiricalVariogramSingleBin])
    -> [VariogramFit]
fitAllKernels kernels (iv, dv, bins) = map (fitOneKernel iv dv bins) kernels

fitOneKernel
    :: IndepVarName
    -> DepVarName
    -> [EmpiricalVariogramSingleBin]
    -> KernelShape
    -> VariogramFit
fitOneKernel iv dv bins kernel =
    let hs    = map (midpoint . _evBin) bins
        ys    = map _evVariance bins
        ws    = map (fromIntegral . _evNrPairs) bins
        -- crude but robust initial guesses
        sill0   = maximum ys
        nug0    = minimum ys
        range0  = median hs
        (nug, sill, range) = optimize (variogramModel kernel) hs ys ws (nug0, sill0, range0)
        loss = weightedSSE (variogramModel kernel) hs ys ws (nug, sill, range)
    in VariogramFit iv dv kernel nug sill (nug/sill) range loss

optimize
  :: VariogramModel
  -> [Double] -> [Double] -> [Double]
  -> (Double,Double,Double)
  -> (Double,Double,Double)
optimize model hs ys ws (nug0, sill0, range0) =
  let
      f :: [Double] -> Double
      f = lossFunLog model hs ys ws
      x0 = map log [max nug0 1e-8, sill0, range0]
      tol      = 1e-6
      maxIters = 500
      step     = replicate 3 1e-2
      (sol, _history) = GSL.minimize GSL.NMSimplex2 tol maxIters step f x0
      [nug, sill, range] = map exp sol
  in (nug, sill, range)

lossFunLog
  :: VariogramModel
  -> [Double] -> [Double] -> [Double]
  -> [Double]
  -> Double
lossFunLog model hs ys ws [lnNug, lnSill, lnRange] =
  let nug   = exp lnNug
      sill  = exp lnSill
      range = exp lnRange
  in sum
     [ w * (y - model nug sill range h) ^ 2
     | (h,y,w) <- zip3 hs ys ws
     ]
lossFunLog _ _ _ _ _ = 1e12

weightedSSE
    :: VariogramModel
    -> [Double] -> [Double] -> [Double]
    -> (Double,Double,Double)
    -> Double
weightedSSE model hs ys ws (nug,sill,range) =
    sum [ w * (y - model nug sill range h)^2 | (h,y,w) <- zip3 hs ys ws ]

midpoint :: (Double,Double,Double) -> Double
midpoint (_,m,_) = m

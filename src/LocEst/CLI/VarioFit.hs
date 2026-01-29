{-# LANGUAGE BangPatterns #-}

module LocEst.CLI.VarioFit where

import           LocEst.Parsers
import           LocEst.Types
import           LocEst.Utils

import           Conduit                  ((.|))
import qualified Data.Conduit             as Con
import qualified Data.Conduit.Combinators as ConC
import           Data.Function            (on)
import           Data.List                (groupBy, sortOn)
import qualified Numeric.GSL.Minimization as GSL
import           System.IO                (hPutStrLn, stderr)

data VarioFitOptions = VarioFitOptions
    { _vfInFile  :: FilePath
    , _vfOutFile :: Maybe FilePath
    --, _vfKernels  :: [KernelShape]
    } deriving Show

runVarioFit :: VarioFitOptions -> IO ()
runVarioFit opts = do
    !bins <- readEmpiricalVariogram (_vfInFile opts)
    hPutStrLn stderr "Fitting theoretical models..."
    let !grouped = groupBins bins
        !fits    = concatMap (fitAllKernels [SquaredExponential, Exponential, Linear]) grouped
    Con.runConduitRes $ ConC.yieldMany fits .| sinkNamedCSV (_vfOutFile opts)
    hPutStrLn stderr "Done"

groupBins :: [EmpiricalVariogramSingleBin] -> [(IndepVarName, DepVarName, [EmpiricalVariogramSingleBin])]
groupBins bins =
    let key b = (_evIndepVar b, _evDepVar b)
        sorted = sortOn key bins
        groups = groupBy ((==) `on` key) sorted
    in map mkGroup groups
    where
      mkGroup (b:bs) = (_evIndepVar b, _evDepVar b, b:bs)
      mkGroup []     = error "groupBins: impossible empty group"

type VariogramModel = Double -> Double -> Double -> Double -> Double
-- v(h) = nugget + sill * f(h / range)

variogramModel :: KernelShape -> VariogramModel
variogramModel SquaredExponential =
    \nug sill range h -> nug + sill * (1 - exp (- ((h * h) / (range * range))))
variogramModel Exponential =
    \nug sill range h -> nug + sill * (1 - exp (- (h / range)))
variogramModel Linear =
    \nug sill range h -> nug + sill * min 1 (h / range)

fitAllKernels :: [KernelShape] -> (IndepVarName, DepVarName, [EmpiricalVariogramSingleBin]) -> [VariogramFit]
fitAllKernels kernels (iv, dv, bins) = map (fitOneKernel iv dv bins) kernels

fitOneKernel
    :: IndepVarName
    -> DepVarName
    -> [EmpiricalVariogramSingleBin]
    -> KernelShape
    -> VariogramFit
fitOneKernel iv dv bins kernel =
    let hs = map ((\(_,m,_) -> m) . _evBin) bins
        ys = map _evVariance bins
        ws = map (fromIntegral . _evNrPairs) bins
        -- initial guesses
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
      lossFun :: [Double] -> Double
      lossFun [lnNug, lnSill, lnRange] = weightedSSE model hs ys ws (exp lnNug, exp lnSill, exp lnRange)
      lossFun _ = 1e12 -- large penalty
      x0 = map log [nug0, sill0, range0]
      tol      = 1e-6
      maxIters = 500
      step     = replicate 3 1e-2
      (sol, _) = GSL.minimize GSL.NMSimplex2 tol maxIters step lossFun x0
  in case map exp sol of
         [nug, sill, range] -> (nug, sill, range)
         _                  -> error "optimize: output missmatch"

weightedSSE
    :: VariogramModel
    -> [Double] -> [Double] -> [Double]
    -> (Double,Double,Double)
    -> Double
weightedSSE model hs ys ws (nug,sill,range) =
    sum [ w * (y - model nug sill range h) ** 2 | (h,y,w) <- zip3 hs ys ws ]

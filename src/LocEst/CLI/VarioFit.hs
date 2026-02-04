{-# LANGUAGE BangPatterns #-}

module LocEst.CLI.VarioFit where

import           LocEst.Parsers
import           LocEst.Types
import           LocEst.Utils

import           Conduit                  ((.|))
import qualified Data.Conduit             as Con
import qualified Data.Conduit.Combinators as ConC
import           Data.Function            (on)
import           Data.List                (groupBy, sortOn, intercalate)
import qualified Numeric.GSL.Minimization as GSL
import           System.IO                (hPutStrLn, stderr)

data VarioFitOptions = VarioFitOptions
    { _vfInFile   :: FilePath
    , _vfKernels  :: [KernelShape]
    , _vfFreeSill :: Bool
    , _vfOutFile  :: Maybe FilePath
    } deriving Show

runVarioFit :: VarioFitOptions -> IO ()
runVarioFit (VarioFitOptions inFile kernels freeSill outFile) = do
    !bins <- readEmpiricalVariogram inFile
    hPutStrLn stderr "Fitting theoretical models..."
    hPutStrLn stderr $ "Selected kernels: " ++ intercalate "," (map show kernels)
    let !grouped = groupBins bins
        !fits    = concatMap (fitAllKernels freeSill kernels) grouped
    Con.runConduitRes $ ConC.yieldMany fits .| sinkNamedCSV outFile
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
-- v(h) = nugget + psill * f(h / range)

variogramModel :: KernelShape -> VariogramModel
variogramModel SquaredExponential =
    \nug psill range h -> nug + psill * (1 - exp (- ((h ** 2) / (range ** 2))))
variogramModel Exponential =
    \nug psill range h -> nug + psill * (1 - exp (- (h / range)))
variogramModel Linear =
    \nug psill range h -> nug + psill * min 1 (h / range)

fitAllKernels :: Bool -> [KernelShape] -> (IndepVarName, DepVarName, [EmpiricalVariogramSingleBin]) -> [VariogramFit]
fitAllKernels freeSill kernels (iv, dv, bins) = map (fitOneKernel freeSill iv dv bins) kernels

fitOneKernel
    :: Bool
    -> IndepVarName
    -> DepVarName
    -> [EmpiricalVariogramSingleBin]
    -> KernelShape
    -> VariogramFit
fitOneKernel freeSill iv dv bins kernel =
    let infinityBin = last bins
        normalBins = init bins
        hs = map ((\(_,m,_) -> m) . _evBin) normalBins
        ys = map _evVariance normalBins
        ws = map (fromIntegral . _evNrPairs) normalBins
        -- optimize function parameters
        (nug, psill, range) =
            if not freeSill
            then
                let fixedSill = _evVariance infinityBin
                    nug0   = minimum ys
                    range0 = median hs
                in optimizeFixedSill (variogramModel kernel) hs ys ws fixedSill (nug0, range0)
            else
                let psill0 = maximum ys
                    nug0   = minimum ys
                    range0 = median hs
                in optimizeFreeSill (variogramModel kernel) hs ys ws (nug0, psill0, range0)
        loss = weightedSSE (variogramModel kernel) hs ys ws (nug, psill, range)
        sill = psill + nug
        nugscaled = nug/sill
    in VariogramFit iv dv kernel nug psill sill nugscaled range loss

        -- 

optimizeFixedSill
  :: VariogramModel
  -> [Double] -> [Double] -> [Double]
  -> Double
  -> (Double,Double)
  -> (Double,Double,Double)
optimizeFixedSill model hs ys ws sill (nug0, range0) =
  let x0       = map log [nug0, range0]
      step     = [1e-2, 1e-2]
      tol      = 1e-6
      maxIters = 500
      (sol, _) = GSL.minimize GSL.NMSimplex2 tol maxIters step lossFun x0
  in case map exp sol of
         [nug, range] -> (nug, sill - nug, range)
         _            -> error "optimize: output missmatch"
  where
      lossFun :: [Double] -> Double
      lossFun [lnNug, lnRange] = weightedSSE model hs ys ws (exp lnNug, sill - exp lnNug, exp lnRange)
      lossFun _ = 1e12 -- large penalty

optimizeFreeSill
  :: VariogramModel
  -> [Double] -> [Double] -> [Double]
  -> (Double,Double,Double)
  -> (Double,Double,Double)
optimizeFreeSill model hs ys ws (nug0, psill0, range0) =
  let x0       = map log [nug0, psill0, range0]
      step     = [1e-2, 1e-2, 1e-2]
      tol      = 1e-6
      maxIters = 500
      (sol, _) = GSL.minimize GSL.NMSimplex2 tol maxIters step lossFun x0
  in case map exp sol of
         [nug, psill, range] -> (nug, psill, range)
         _                   -> error "optimize: output missmatch"
  where
      lossFun :: [Double] -> Double
      lossFun [lnNug, lnPSill, lnRange] = weightedSSE model hs ys ws (exp lnNug, exp lnPSill, exp lnRange)
      lossFun _ = 1e12 -- large penalty

weightedSSE
    :: VariogramModel
    -> [Double] -> [Double] -> [Double]
    -> (Double,Double,Double)
    -> Double
weightedSSE model hs ys ws (nug,psill,range) =
    sum [ w * (y - model nug psill range h) ** 2 | (h,y,w) <- zip3 hs ys ws ]

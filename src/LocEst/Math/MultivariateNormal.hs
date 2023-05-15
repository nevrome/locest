{-# LANGUAGE FlexibleContexts #-}

module LocEst.Math.MultivariateNormal (
    dnormMulti
    ) where

import           Data.Maybe                    (fromJust)
import           Foreign.Storable              (Storable)
import qualified Numeric.LinearAlgebra.Data    as HD
import qualified Numeric.LinearAlgebra.HMatrix as H

-- mu: mean on each pc
-- sigma: sd on each pc
-- x: coordinates for point-of-interest on each pc
dnormMulti :: [Double] -> [Double] -> [Double] -> Double
dnormMulti mu sigma x = normalPDF (makeMu mu) (makeSigma sigma) (makeX x)

makeX :: [Double] -> H.Vector Double
makeX = HD.fromList

makeMu :: [Double] -> H.Vector Double
makeMu = HD.fromList

makeSigma :: [Double] -> H.Herm Double
makeSigma xs = H.sym $ HD.diagl xs

-- taken from https://github.com/idontgetoutmuch/random-fu-multivariate
normalPDF :: (H.Numeric a, H.Field a, H.Indexable (H.Vector a) a, Num (H.Vector a)) =>
             H.Vector a -> H.Herm a -> H.Vector a -> a
normalPDF mu sigma x = exp $ normalLogPDF mu sigma x

-- taken from https://github.com/idontgetoutmuch/random-fu-multivariate
normalLogPDF :: (H.Numeric a, H.Field a, H.Indexable (H.Vector a) a, Num (H.Vector a)) =>
                 H.Vector a -> H.Herm a -> H.Vector a -> a
normalLogPDF mu bigSigma x = - H.sumElements (H.cmap log (diagonals dec))
                              - 0.5 * fromIntegral (H.size mu) * log (2 * pi)
                              - 0.5 * s
    where
        dec = fromJust $ H.mbChol bigSigma
        t = fromJust $ H.linearSolve (H.tr dec) (H.asColumn $ x - mu)
        u = H.cmap (\v -> v * v) t
        s = H.sumElements u

-- taken from https://github.com/idontgetoutmuch/random-fu-multivariate
diagonals :: (Storable a, H.Element t, H.Indexable (H.Vector t) a) =>
             H.Matrix t -> H.Vector a
diagonals m = H.fromList (map (\i -> m H.! i H.! i) [0..n-1])
  where
    n = max (H.rows m) (H.cols m)

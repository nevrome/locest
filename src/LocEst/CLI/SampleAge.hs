{-# LANGUAGE BangPatterns #-}

module LocEst.CLI.SampleAge where

import LocEst.Parsers
import LocEst.Types
import LocEst.Utils

import qualified System.Random as R
import           System.IO       (hPutStrLn, stderr)
import qualified Currycarbon.SumCalibration as C14
import qualified Currycarbon as C14
import qualified Currycarbon.Utils as C14
import qualified Currycarbon.Calibration.Calibration as C14
import Data.Either (rights)


data SampleAgeOptions = SampleAgeOptions {
    _spfsInObservationFile :: FilePath,
    _spfsOutFile           :: FilePath
}

runSampleAge :: SampleAgeOptions -> IO ()
runSampleAge (SampleAgeOptions inObsFile _) = do
    !allObservationAges <- readObservationAges inObsFile
    let gen = R.mkStdGen 1234
    hPutStrLn stderr $ show $ rights $ map (drawSamplesFromRawAge gen) allObservationAges

    hPutStrLn stderr "Done"

drawSamplesFromRawAge :: R.StdGen -> ObservationAge -> Either LOCESTException [YearBCAD]
drawSamplesFromRawAge randomNumberGenerator (ObservationAge obsID (CurryCarbonCalExpr c14CalExpr)) =
    case C14.evalCalExpr C14.defaultCalConf C14.intcal20 c14CalExpr of
        Left err -> Left $ NormalException $ "currycarbon: " ++ C14.renderCurrycarbonException err
        Right calPDF  -> Right $ C14.sampleAgesFromCalPDF randomNumberGenerator 3 calPDF

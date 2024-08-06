module LocEst.ReorderVars where

import           LocEst.Types

import qualified Data.Vector               as V

reorderDistanceFilterThresholds :: [String] -> DistanceFilterThresholds -> DistanceFilterThresholds
reorderDistanceFilterThresholds _ f@(SpaceTimeFilterThresholds _ _) = f
reorderDistanceFilterThresholds indepVarsWanted (ArbitraryDimFilterThresholds minFilter maxFilter) =
    ArbitraryDimFilterThresholds
        (fmap (reorderAndFilter indepVarsWanted) minFilter)
        (fmap (reorderAndFilter indepVarsWanted) maxFilter)

reorderVarsInObs :: [String] -> [String] -> V.Vector Observation -> V.Vector Observation
reorderVarsInObs depVarsWanted indepVarsWanted = V.map reorderVarsInOneObs
    where
        reorderVarsInOneObs :: Observation -> Observation
        -- spatiotemporal case
        reorderVarsInOneObs o@(Observation _ _ (HyperPos std@(IndepSpatTempPos _) depInObs) _) =
            let depRes = reorderAndFilter depVarsWanted depInObs
            in o { _obsPos = HyperPos std depRes }
        -- arbitrary dimension case
        reorderVarsInOneObs o@(Observation _ _ (HyperPos (IndepArbitraryDimPos indepInObs) depInObs) _) =
            let depRes   = reorderAndFilter depVarsWanted depInObs
                indepRes = reorderAndFilter indepVarsWanted indepInObs
            in o { _obsPos = HyperPos (IndepArbitraryDimPos indepRes) depRes }


reorderVarsInArbitraryPos :: [String] -> V.Vector ValuesPerIndepVar -> V.Vector ValuesPerIndepVar
reorderVarsInArbitraryPos indepVarsWanted = V.map (reorderAndFilter indepVarsWanted)

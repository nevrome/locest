module LocEst.ReorderVars where

import           LocEst.Types

import qualified Data.Vector  as V

-- the filtering takes place, because it must be possible to control the variables of interest
-- in the kernel definition
-- TODO: Remember what's the purpose of the ordering - I forgot

reorderDistanceFilterThresholds :: [String] -> DistanceFilterThresholds -> DistanceFilterThresholds
reorderDistanceFilterThresholds _ f@(SpaceTimeFilterThresholds _ _) = f
reorderDistanceFilterThresholds indepVarsWanted (ArbitraryDimFilterThresholds minFilter maxFilter) =
    ArbitraryDimFilterThresholds
        (fmap (filterByKey indepVarsWanted) minFilter)
        (fmap (filterByKey indepVarsWanted) maxFilter)

filterVarsInObs :: [String] -> [String] -> V.Vector Observation -> V.Vector Observation
filterVarsInObs depVarsWanted indepVarsWanted = V.map handleOne
    where
        handleOne :: Observation -> Observation
        -- spatiotemporal case
        handleOne o@(Observation _ _ (HyperPos std@(IndepSpatTempPos _) depInObs) _) =
            let depRes = filterByKey depVarsWanted depInObs
            in o { _obsPos = HyperPos std depRes }
        -- arbitrary dimension case
        handleOne o@(Observation _ _ (HyperPos (IndepArbitraryDimPos indepInObs) depInObs) _) =
            let depRes   = filterByKey depVarsWanted depInObs
                indepRes = filterByKey indepVarsWanted indepInObs
            in o { _obsPos = HyperPos (IndepArbitraryDimPos indepRes) depRes }

reorderVarsInArbitraryPos :: [String] -> V.Vector ValuesPerIndepVar -> V.Vector ValuesPerIndepVar
reorderVarsInArbitraryPos indepVarsWanted = V.map (filterByKey indepVarsWanted)

module LocEst.ReorderVars where

import           LocEst.Types

import qualified Data.Vector               as V

reorderVarsInObs :: [String] -> [String] -> V.Vector Observation -> V.Vector Observation
reorderVarsInObs depVarsWanted indepVarsWanted = V.map reorderVarsInOneObs
    where
        reorderVarsInOneObs :: Observation -> Observation
        -- spatiotemporal case
        reorderVarsInOneObs o@(Observation _ _ (HyperPos std@(IndepSpatTempPos _) depInObs) _) =
            let depRes = reorderAndFilter depInObs depVarsWanted
            in o { _obsPos = HyperPos std depRes }
        -- arbitrary dimension case
        reorderVarsInOneObs o@(Observation _ _ (HyperPos (IndepArbitraryDimPos indepInObs) depInObs) _) =
            let depRes   = reorderAndFilter depInObs depVarsWanted
                indepRes = reorderAndFilter indepInObs indepVarsWanted
            in o { _obsPos = HyperPos (IndepArbitraryDimPos indepRes) depRes }


reorderVarsInArbitraryPos :: [String] -> V.Vector ArbitraryDimPos -> V.Vector ArbitraryDimPos
reorderVarsInArbitraryPos indepVarsWanted = V.map reorderVarsInOneArbitraryDimPos
    where
        reorderVarsInOneArbitraryDimPos :: ArbitraryDimPos -> ArbitraryDimPos
        reorderVarsInOneArbitraryDimPos x = reorderAndFilter x indepVarsWanted
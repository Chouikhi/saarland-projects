{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module TrulyLiveVariables ( performAnalysis
                          , performOptimization
                          , prettyState
                          
                         -- for testing purposes
                          , edgeEffectTLV) where

import AnalysisBase
import FixPointAlgorithmBase
import Program
import Data.Maybe
import Data.List ((\\), union, intersect, deleteBy, intersperse)

instance Carrier CarrierTLV where

type CarrierTLV = [Var]
type StateTLV = State CarrierTLV

prettyState :: StateTLV -> String
prettyState plvs = unlines $ map prettyLV plvs
  where
    prettyLV :: (Point, CarrierTLV) -> String
    prettyLV (p, lvs) = "    " ++ prettyPoint p ++ " : {"
                    ++ (foldr (++) "" (intersperse ", " $ map prettyVar lvs))
                    ++ "}"

fromJustX str m = if isJust m then fromJust m else error(str)

edgeEffectTLV :: Label -> CarrierTLV -> CarrierTLV
edgeEffectTLV lbl inp = (inp \\ (maybeToList written)) `union`
                        if isNothing written || fromJustX "eetlv" written `elem` inp
                        then read
                        else []
  where
    (read, written) = labelVars lbl

initStateTLV prog = [(p, []) | p <- programPoints prog]

analysis = Analysis
  { combine = union
  , direction = Backward
  , edgeEffect = labelToEdge edgeEffectTLV
  }

performAnalysis :: (FixPointAlgorithm CarrierTLV) -> Program -> StateTLV
performAnalysis fpa prog = fpa analysis prog (initStateTLV prog)

performOptimization :: Program -> (StateTLV) -> Program
performOptimization = undefined

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module TrulyLiveVariables ( performAnalysis
                          , performOptimization
                          
                         -- for testing purposes
                          , edgeEffectTLV) where

import AnalysisBase
import FixPointAlgorithmBase
import Program
import Data.Maybe
import Util
import Data.List ((\\), union, intersect, deleteBy, intersperse, sort)

type CarrierTLV = [Var]
type StateTLV = State CarrierTLV

instance Carrier CarrierTLV where
  prettyCarrier lvs = foldr (++) "" (intersperse ", " $ map prettyVar lvs)
  normalizeCarrier = sort

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
  -- TODO: Add check for monotonicity here.
  , fix = curry snd
  }

performAnalysis :: FixPointAlgorithm CarrierTLV -> Program -> StateTLV
performAnalysis fpa prog = fpa analysis prog (initStateTLV prog)

performOptimization :: Program -> StateTLV -> Program
performOptimization prog live = map (\edge -> if shouldRemove edge
                                              then edge { label = Nop }
                                              else edge)
                                    prog
  where
    -- mkNop e = e { label = Nop } -- (Edge p1 _ p2) = (Edge p1 Nop p2)
    shouldRemove (Edge _ lbl p2) = let writeVar = snd $ labelVars lbl
                                   in if isJust $ writeVar
                                      then fromJust writeVar `notElem` jLookup p2 live
                                      else False

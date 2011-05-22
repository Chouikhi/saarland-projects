module TrulyLiveVariables where

import Program
import Data.Maybe
import Data.List ((\\), union, intersect, deleteBy, intersperse)

type TLVCarrier = [Var]
type TLVState = [(Point, TLVCarrier)]

prettyState :: TLVState -> String
prettyState plvs = unlines $ map prettyLV plvs
  where
    prettyLV :: (Point, TLVCarrier) -> String
    prettyLV (p, lvs) = "    " ++ prettyPoint p ++ " : {" ++ (foldr (++) "" (intersperse ", " $ map prettyVar lvs)) ++ "}"

combineTLV = union  -- if a var is truly live on at least one path it is truly live at the top
initTLV = []  -- no variables are live at the end

fromJustX str m = if isJust m then fromJust m else error(str)

edgeEffectTLV :: Label -> TLVCarrier -> TLVCarrier
edgeEffectTLV lbl inp = (inp \\ (maybeToList written)) `union`
                        if isNothing written || fromJustX "eetlv" written `elem` inp
                        then read
                        else []
  where
    (read, written) = labelVars lbl

evalTLV :: Program -> Point -> TLVState -> (TLVState, Bool)
evalTLV prog point state = if oldd == newd then (state, False) else (newState, True)
  where
    edgeEffects = map (uncurry edgeEffectTLV) depData
    depData = map (\(lbl, p) -> (lbl, fromJustX "etlv1" $ lookup p state)) depPts
    depPts = evalDependantEdges prog Backward point
    oldd = fromJustX "etlv2" $ lookup point state
    newd = foldr combineTLV initTLV edgeEffects
    newState = (point, newd) : filter ((/= point) . fst) state
                    
-- evalTLV :: Program -> Point -> TLVState -> TLVCarrier
-- evalTLV prog point state =
--     where
--       label = lookup point prog

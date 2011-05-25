module AnalysisBase where

import Program
import Data.Maybe

class (Show c, Eq c, Ord c) => Carrier c where

data (Carrier carrier) => Analysis carrier = Analysis
  { combine :: carrier -> carrier -> carrier
  , init :: carrier
  , direction :: Direction
  , edgeEffect :: Edge -> carrier -> carrier
  }

type {- (Carrier carrier) => -} State carrier = [(Point, carrier)]


labelToEdge :: (Carrier carrier) => (Label -> carrier -> carrier) -> Edge -> carrier -> carrier
labelToEdge labelEffect (Edge _ lbl _) = labelEffect lbl

eval :: (Carrier carrier) => Analysis carrier -> Program -> Point -> State carrier -> (State carrier, Bool)
eval asys prog point state = if oldd == newd then (state, False) else (newState, True)
  where
    edgeEffects = map (uncurry $ edgeEffect asys) depData
    depData = map (\(e, p) -> (e, fromJust $ lookup p state)) depPts
    depPts = evalDependantEdges prog Backward point
    oldd = fromJust $ lookup point state
    newd = foldr (combine asys) (AnalysisBase.init asys) edgeEffects
    newState = (point, newd) : filter ((/= point) . fst) state

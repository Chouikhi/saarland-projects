module AnalysisBase where

import Program
import Data.Maybe
import Data.List (sort)
import Util (sndMap)

class (Show c, Eq c, Ord c) => Carrier c where
  prettyState :: State c -> String
  prettyState s = unlines $ map prettyPC $ sort s
    where prettyPC (p, c) = "    " ++ prettyPoint p ++ " : {"
                          ++ prettyCarrier c ++ "}"

  prettyCarrier :: c -> String
  prettyCarrier = undefined

  normalizeState :: State c -> State c
  normalizeState s = sort $ sndMap normalizeCarrier s

  normalizeCarrier :: c -> c
  normalizeCarrier = undefined

data (Carrier carrier) => Analysis carrier = Analysis
  { combine :: carrier -> carrier -> carrier
  , direction :: Direction
  , edgeEffect :: Edge -> carrier -> carrier
  , fix :: carrier -> carrier -> carrier
  }

type {- (Carrier carrier) => -} State carrier = [(Point, carrier)]

labelToEdge :: (Carrier carrier) => (Label -> carrier -> carrier) -> Edge
                                 -> carrier -> carrier
labelToEdge labelEffect (Edge _ lbl _) = labelEffect lbl

eval :: (Carrier carrier) => Analysis carrier -> Program -> Point
                          -> State carrier -> (State carrier, Bool)
eval asys prog point state = if oldd == fixedNew then (state, False) else (newState, True)
  where
    edgeEffects = map (uncurry $ edgeEffect asys) depData
    depData = map (\(e, p) -> (e, fromJust $ lookup p state)) depPts
    depPts = evalDependantEdges prog (direction asys) point
    oldd = fromJust $ lookup point state
    newd = if edgeEffects == []
           then fromJust $ lookup point state
           else foldr1 (combine asys) edgeEffects
    fixedNew = fix asys oldd newd
    newState = (point, fixedNew) : filter ((/= point) . fst) state


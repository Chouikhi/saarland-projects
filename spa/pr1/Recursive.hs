{-# LANGUAGE ScopedTypeVariables #-}
module Recursive( recursive
                ) where

import AnalysisBase
import Data.List (union, (\\))
import Data.Maybe (fromJust)
import Program
import Util

type DepList = [(Point, [Point])]
type StableList = [Point]
type RecState c = (DepList, StableList, (State c))



recursive :: forall c . Carrier c => (Analysis c) -> Program
                                  -> (State c) -> (State c)
recursive asys prog initState = third $ foldr solve
                                              ( [(p, []) | p <- allPoints]
                                              , []
                                              , initState)
                                              allPoints
  where
    allPoints = programPoints prog
    solve :: Point -> (RecState c) -> (RecState c)
    solve p rs@(is, ss, st) = if p `elem` ss
                              then rs
                              else nrs
      where
        rs'@(is', ss', st') = foldr (\dp rs -> depEval dp p rs)
                                    (is, p:ss, st)
                                    (map snd (evalDependantEdges prog (direction asys) p))
        (ns, change) = eval asys prog p st'
        rs2 = if change
              then ( aeUpd p [] is'
                   , ss' \\ (fromJust (lookup p is'))
                   , ns)
              else rs'
        nrs = foldr (\dp rs -> solve dp rs)
                    rs2
                    (fromJust $ lookup p is')

    depEval :: Point -> Point -> (RecState c) -> (RecState c)
    depEval p neededFor rs@(is, ss, st) = let (is', ss', st') = solve p rs
                                          in  (aeUpdF p (union [neededFor]) is', ss', st')

    third (_, _, z) = z
    -- upd a b al = upd2 a (\_ -> b) al
    -- upd2 :: (Eq a) => a -> (b -> b) -> [(a, b)] -> [(a, b)]
    -- upd2 a f al = let b = fromJust $ lookup a al
    --                   tal = filter (\(ax, _) -> ax /= a) al
    --               in  (a, f b) : tal

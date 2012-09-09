module Worklist( worklist
               ) where

import AnalysisBase
import Program
import Data.List(union)

worklist :: (Carrier c) => Analysis c -> Program -> State c -> State c
worklist asys prog initState = step allPoints initState
  where
    allPoints = programPoints prog
    step [] state = state
    step (p:wl) state = if change
                        then step (wl `union` recalc) state'
                        else step wl state
      where
        (state', change) = eval asys prog p state
        recalc = map snd $ resultDependantEdges prog (direction asys) p

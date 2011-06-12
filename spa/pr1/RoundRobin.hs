module RoundRobin( roundRobin
                 ) where

import AnalysisBase
import Program
import Data.List

roundRobin :: (Carrier c) => (Analysis c) -> Program -> (State c) -> (State c)
roundRobin asys prog initState = step points initState False 0
  where
    points = programPoints prog
    step [] state False _ = state
    -- step [] state True 2 = state
    step [] state True iter  = step points state False (iter + 1)
    step (p:ps) state changed iter = step ps state' changed' iter
      where
        (state', change) = eval asys prog p state
        changed' = changed || change

module RoundRobin( roundRobin
                 ) where

import AnalysisBase
import Program
import Data.List

roundRobin :: (Carrier c) => (Analysis c) -> Program -> (State c) -> (State c)
roundRobin asys prog initState = step points initState False
  where
    points = programPoints prog
    vars = programVars prog
    step [] state False = state
    step [] state True  = step points state False
    step (p:ps) state changed = step ps state' changed'
      where
        (state', change) = eval asys prog p state
        changed' = changed || change

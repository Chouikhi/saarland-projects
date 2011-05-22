module RoundRobin( roundRobin
                 ) where

import TrulyLiveVariables

import Data.List
import Program

roundRobin :: Program -> TLVState
roundRobin prog = step points init False
  where
    points = programPoints prog
    init = [(p, vars) | p <- points]
    vars = programVars prog
    step :: [Point] -> TLVState -> Bool -> TLVState
    step [] state False = state
    -- step [] state True  = state
    step [] state True  = step points state False
    step (p:ps) state changed = step ps state' changed'
      where
        (state', change) = evalTLV prog p state
        changed' = changed || change

module Main where

import Data.List (sort)

import Parser
import Program
import RoundRobin
import Worklist
import Recursive
import qualified TrulyLiveVariables
import qualified AvailableExpressions
import qualified IntervalAnalysis

-- main = getContents >>= putStr . TrulyLiveVariables.prettyState . sort . (TrulyLiveVariables.performAnalysis roundRobin) . program . parseAnalysis . lexer
-- main = getContents >>= putStr . TrulyLiveVariables.prettyState . sort . (TrulyLiveVariables.performAnalysis recursive) . program . parseAnalysis . lexer
-- main = getContents >>= putStr . TrulyLiveVariables.prettyState . sort . (TrulyLiveVariables.performAnalysis worklist) . program . parseAnalysis . lexer
-- main = getContents >>= putStr . AvailableExpressions.prettyState . sort . (AvailableExpressions.performAnalysis recursive) . program . parseAnalysis . lexer
-- main = getContents >>= putStr . AvailableExpressions.prettyState . sort . AvailableExpressions.initStateAE . program . parseAnalysis . lexer
-- main = getContents >>= putStr . IntervalAnalysis.prettyState . sort . (IntervalAnalysis.performAnalysis recursive) . program . parseAnalysis . lexer

-- main = getContents >>= (\inp ->
--           let prog = (program . parseAnalysis . lexer) inp
--               asysRes = TrulyLiveVariables.performAnalysis worklist prog
--               optRes = TrulyLiveVariables.performOptimization prog asysRes
--           in  putStr (prettyProgram optRes))

-- main = getContents >>= (\inp ->
--           let prog = (program . parseAnalysis . lexer) inp
--               asysRes = IntervalAnalysis.performAnalysis worklist prog
--               optRes = IntervalAnalysis.performOptimization prog asysRes
--           in  putStr (prettyProgram optRes))

-- XXX: Call appropriate stuff based on input.
main = getContents >>= (\inp ->
          let prog = (program . parseAnalysis . lexer) inp
              asysRes = AvailableExpressions.performAnalysis worklist prog
              optRes = AvailableExpressions.performOptimization prog asysRes
--           in  putStr $ show optRes)
          in  putStr (prettyProgram optRes))

-- >>= putStr . TrulyLiveVariables.prettyState . sort . (TrulyLiveVariables.performAnalysis worklist) . program . parseAnalysis . lexer

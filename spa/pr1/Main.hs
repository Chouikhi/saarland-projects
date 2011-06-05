module Main where

import Data.List (sort)

import Parser
import Program
import RoundRobin
import Worklist
import qualified TrulyLiveVariables
import qualified AvailableExpressions

main = getContents >>= putStr . TrulyLiveVariables.prettyState . sort . (TrulyLiveVariables.performAnalysis roundRobin) . program . parseAnalysis . lexer
-- main = getContents >>= putStr . TrulyLiveVariables.prettyState . sort . (TrulyLiveVariables.performAnalysis worklist) . program . parseAnalysis . lexer
-- main = getContents >>= putStr . AvailableExpressions.prettyState . sort . (AvailableExpressions.performAnalysis roundRobin) . program . parseAnalysis . lexer
-- main = getContents >>= putStr . AvailableExpressions.prettyState . sort . AvailableExpressions.initStateAE . program . parseAnalysis . lexer

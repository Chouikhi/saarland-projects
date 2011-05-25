module Main where

import Data.List (sort)

import Parser
import Program
import RoundRobin
import qualified TrulyLiveVariables

main = getContents >>= putStr . TrulyLiveVariables.prettyState . sort . (TrulyLiveVariables.performAnalysis roundRobin) . program . parseAnalysis . lexer

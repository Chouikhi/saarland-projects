module Main where

import Data.List (sort)

import Parser
import Program
import RoundRobin
import TrulyLiveVariables

main = getContents >>= putStr . prettyState . sort . roundRobin . program . parseAnalysis . lexer

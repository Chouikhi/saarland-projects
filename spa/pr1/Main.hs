{-# LANGUAGE RankNTypes, GADTs #-}

module Main where

import Data.List (sort)

import Parser
import Program
import FixPointAlgorithmBase
import AnalysisBase
import Util
import qualified RoundRobin
import qualified Worklist
import qualified Recursive
import qualified TrulyLiveVariables as TLV
import qualified AvailableExpressions as AE
import qualified IntervalAnalysis as IA

fixpointMap :: [(String, AnyFixPointAlgorithm)]
fixpointMap = [ ("Round_Robin", WrapFixPointAlgorithm RoundRobin.roundRobin)
              , ("Worklist", WrapFixPointAlgorithm Worklist.worklist)
              , ("Recursive", WrapFixPointAlgorithm Recursive.recursive)
              ]
data AnalyzerOptimizerPair where
  WrapAnalyzerOptimizerPair :: forall c. Carrier c
                            => (FixPointAlgorithm c -> Program -> State c)
                            -> (Program -> State c -> Program)
                            -> AnalyzerOptimizerPair

analysisMap :: [(String, AnalyzerOptimizerPair)]
analysisMap = [ ( "Available_Expressions"
                , WrapAnalyzerOptimizerPair AE.performAnalysis
                                            AE.performOptimization
                )
              , ( "Truly_Live_Variables"
                , WrapAnalyzerOptimizerPair TLV.performAnalysis
                                            TLV.performOptimization
                )
              , ( "Interval_Analysis"
                , WrapAnalyzerOptimizerPair IA.performAnalysis
                                            IA.performOptimization
                )
              ]

main = getContents >>= (\inp ->
          let parsedInp = (parseAnalysis . lexer) inp
              prog = program parsedInp
              analysisName = getName $ analysis parsedInp
              fpalgName = getName $ algorithm parsedInp
              action = getName $ output parsedInp
              strRes = case jLookup analysisName analysisMap of
                         (WrapAnalyzerOptimizerPair analyzer optimizer) ->
                           let
                             fpalg = case jLookup fpalgName fixpointMap of
                                       (WrapFixPointAlgorithm a) -> a
                             asysRes = analyzer fpalg prog
                             optRes = optimizer prog asysRes
                             strRes = if action == "Analysis"
                                      then pretty asysRes
                                      else prettyProgram optRes
                           in strRes
          in  putStr strRes)

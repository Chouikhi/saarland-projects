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
import qualified TrulyLiveVariables
import qualified AvailableExpressions
import qualified IntervalAnalysis

fixpointMap :: [(String, AnyFixPointAlgorithm)]
fixpointMap = [ ("Round_Robin", WrapFixPointAlgorithm RoundRobin.roundRobin)
              , ("Worklist", WrapFixPointAlgorithm Worklist.worklist)
              , ("Recursive", WrapFixPointAlgorithm Recursive.recursive)
              ]
data AnalizerOptimizerPair where
  WrapAnalizerOptimizerPair :: forall c. Carrier c
                            => ( FixPointAlgorithm c -> Program -> State c
                               , Program -> State c -> Program
                               ) -> AnalizerOptimizerPair

analysisMap :: [(String, AnalizerOptimizerPair)]
analysisMap = [ ( "Available_Expressions"
                , WrapAnalizerOptimizerPair 
                  ( AvailableExpressions.performAnalysis
                  , AvailableExpressions.performOptimization
                  )
                )
              , ( "Truly_Live_Variables"
                , WrapAnalizerOptimizerPair
                  ( TrulyLiveVariables.performAnalysis
                  , TrulyLiveVariables.performOptimization
                  )
                )
              , ( "Interval_Analysis"
                , WrapAnalizerOptimizerPair
                  ( IntervalAnalysis.performAnalysis
                  , IntervalAnalysis.performOptimization
                  )
                )
              ]

main = getContents >>= (\inp ->
          let parsedInp = (parseAnalysis . lexer) inp
              prog = program parsedInp
              analysisName = getName $ analysis parsedInp
              fpalgName = getName $ algorithm parsedInp
              action = getName $ output parsedInp
              strRes = case jLookup analysisName analysisMap of
                         WrapAnalizerOptimizerPair (analyzer, optimizer) ->
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

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

fixpointMap :: [(String, GenericFixPointAlgorithm)]
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

main = do inp <- getContents
          let parsedInp = (parseAnalysis . lexer) inp
              prog = program parsedInp
              analysisName = getName $ analysis parsedInp
              fpalgName = getName $ algorithm parsedInp
              action = getName $ output parsedInp
              (header, strRes) = case jLookup analysisName analysisMap of
                                   (WrapAnalyzerOptimizerPair analyzer optimizer) ->
                                     let
                                       fpalg = instantiateFixPointAlgorithm $ jLookup fpalgName fixpointMap
                                       asysRes = analyzer fpalg prog
                                       optRes = optimizer prog asysRes
                                     in if action == "Analysis"
                                        then ("ANALYSIS_RESULTS", prettyState asysRes)
                                        else ("TRANSFORMATION_RESULT", prettyProgram optRes)
          putStrLn header
          putStr strRes

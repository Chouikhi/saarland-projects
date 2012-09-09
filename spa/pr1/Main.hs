{-# LANGUAGE RankNTypes, GADTs #-}

module Main where

import Data.List (sort)

import Parser
import Program
import FixPointAlgorithmBase
import AnalysisBase
import Util
import GHC.Exts (the)
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
headerMap = [ ("Analysis", "ANALYSIS_RESULTS")
            , ("Transformation", "TRANSFORMATION_RESULT")
            ]

-- weather to compare the results from all fix point algorithms to make sure
-- everything is correct
testAllAlgorithms = True

analyze analyzer prefFpAlgName prog = the $ map normalizeState allStates
  where
    allStates = map (\alg -> analyzer (instantiateFixPointAlgorithm alg) prog) 
              $ if testAllAlgorithms
                then map snd fixpointMap
                else [jLookup prefFpAlgName fixpointMap]

main = do inp <- getContents
          let parsedInp = (parseAnalysis . lexer) inp
              prog = program parsedInp
              analysisName = getName $ analysis parsedInp
              fpalgName = getName $ algorithm parsedInp
              action = getName $ output parsedInp
              strRes = case jLookup analysisName analysisMap of
                         (WrapAnalyzerOptimizerPair analyzer optimizer) ->
                           let
                             asysRes = analyze analyzer fpalgName prog
                             optRes = optimizer prog asysRes
                           in if action == "Analysis"
                              then prettyState asysRes
                              else prettyProgram optRes
          putStrLn $ jLookup action headerMap
          putStr strRes

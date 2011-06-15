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

-- performAnalysis :: (FixPointAlgorithm CarrierAE) -> Program -> StateAE
-- performOptimization :: Program -> StateAE -> Program
-- transformProgram :: (Carrier c) => ((FixPointAlgorithm c) -> Program -> State c)
--                                 -> (Program -> State c -> Program)
--                                 -> (forall cx. Carrier cx => FixPointAlgorithm cx)
--                                 -> Program
--                                 -> Program
-- transformProgram analyzer transformer fpalg prog = transformer prog (analyzer fpalg prog)

-- showAnalysis :: (Carrier c, StateCls (State c)) => ((FixPointAlgorithm c) -> Program -> State c)
--                                                 -> AnyFixPointAlgorithm --(forall cx. Carrier cx => FixPointAlgorithm cx)
--                                                 -> Program
--                                                 -> String
-- showAnalysis analyzer (AFPA fpalg) prog = pretty $ analyzer fpalg prog

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
fixpointMap :: [(String, AnyFixPointAlgorithm)]
fixpointMap = [ ("Round_Robin", WrapFixPointAlgorithm RoundRobin.roundRobin)
              , ("Worklist", WrapFixPointAlgorithm Worklist.worklist)
              , ("Recursive", WrapFixPointAlgorithm Recursive.recursive)
              ]
data SomeAnalizer where
  WrapAnalizer :: forall c. Carrier c
               => (FixPointAlgorithm c -> Program -> State c) -> SomeAnalizer

data SomeOptimizer where
  WrapOptimizer :: forall c. Carrier c
                => (Program -> State c -> Program) -> SomeOptimizer

-- analysisMap = [ ("Available_Expressions", showAnalysis
analysisMap :: [(String, (SomeAnalizer, SomeOptimizer))]
analysisMap = [ ( "Available_Expressions"
                , (WrapAnalizer AvailableExpressions.performAnalysis
                  , WrapOptimizer AvailableExpressions.performOptimization
                  )
                )
              , ( "Truly_Live_Variables"
                , ( WrapAnalizer TrulyLiveVariables.performAnalysis
                  , WrapOptimizer TrulyLiveVariables.performOptimization
                  )
                )
              , ( "Interval_Analysis"
                , ( WrapAnalizer IntervalAnalysis.performAnalysis
                  , WrapOptimizer IntervalAnalysis.performOptimization
                  )
                )
              ]

main = getContents >>= (\inp ->
          let parsedInp = (parseAnalysis . lexer) inp
              prog = program parsedInp
              analysisName = getName $ analysis parsedInp
              fpalgName = getName $ algorithm parsedInp
              -- (WrapAnalizer analyzer, WrapOptimizer optimizer) = jLookup analysisName analysisMap
              (analyzer, optimizer) = case jLookup analysisName analysisMap of
                                        (WrapAnalizer a, WrapOptimizer o) -> (a, o)
              (WrapFixPointAlgorithm fpalg) = jLookup fpalgName fixpointMap
              action = getName $ output parsedInp
              asysRes = analyzer fpalg prog
              optRes = optimizer prog asysRes
          in  putStr (prettyProgram optRes))

-- >>= putStr . TrulyLiveVariables.prettyState . sort . (TrulyLiveVariables.performAnalysis worklist) . program . parseAnalysis . lexer

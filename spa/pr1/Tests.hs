module Tests where

import Test.HUnit

import Data.List (sort)

import TrulyLiveVariables
import AnalysisBase
import AvailableExpressions
import RoundRobin
import Program
import Parser

sParseExpr = parseExpr . lexer
sParseLabel = parseLabel . lexer
sParseEdge = parseEdge . lexer
sParseProgram = parseProgram . lexer

p = Point

assertEqSet str e1 e2 = assertEqual str (sort e1) (sort e2)

testProg1 = unlines [
    "START  x = 2;           P1",
    "P1     y = 36;          P2",
    "P2     t = x * 21;      P3",
    "P3     Pos(y > 0)       P4",
    "P3     Neg(y > 0)       P5",
    "P4     y = y - 1;       P6",
    "P6     t = x * 21;      P7",
    "P7     ;                P3",
    "P5     x = 5;           P8",
    "P8     M[0] = x + t;    END"]

testProg1Parsed = sParseProgram testProg1

testProg2 = unlines [
  "START x = y + 1; END"]

testProg2Parsed = sParseProgram testProg2


-- testProg1NTExprs = map sParseExpr [ "x * 21", "y > 0", "y - 1", "x + t" ]


root = TestList
      [ TestLabel "Parser" $ TestList
        [ TestCase $ assertEqual "nop" Nop (sParseLabel ";")
        , TestCase $ assertEqual "assign"
                                 (Assign (Var "x") (AExpr $ AtomVar (Var "y")))
                                 (sParseLabel "x = y;")
        , TestCase $ assertEqual "load"
                                 (Load (Var "x") (BExpr (AExpr (AtomConst 23))
                                                        Times
                                                        (AExpr (AtomVar (Var "y")))))
                                 (sParseLabel "x = M[23*y];") 
        ]

      , TestLabel "TrulyLiveVariables Edge effects" $ TestList
        [ TestCase $ assertEqual "nop" (edgeEffectTLV Nop [(Var "x")]) [(Var "x")]
        , TestCase $ assertEqual "assign to live"
                                [Var "y"]
                                (edgeEffectTLV
                                  (sParseLabel "x = y;")
                                  [Var "x"])
        , TestCase $ assertEqual "assign to dead"
                                 [Var "y"]
                                 (edgeEffectTLV (sParseLabel "x = y;")
                                                [Var "x", Var "y"])
        , TestCase $ assertEqSet "assign const" (edgeEffectTLV (sParseLabel "x = 5;") [Var "x", Var "t"])
                                                [Var "t"]
        ]

      , TestLabel "TrulyLiveVariables eval" $ TestList
        [
        ]

      , TestLabel "AvailableExpressions Edge effects" $ TestList
        [ TestCase $ assertEqual "nop"
                                 [ (Just (p "P1", p "P2"), sParseExpr "x + 3") ]
                                 (edgeEffectAE (sParseEdge "P2 ; P3")
                                               [(Just (p "P1", p "P2"), sParseExpr "x + 3")])
        , TestCase $ assertEqSet "compute already available expr"
                                 [ (Just (p "P1", p "P2"), sParseExpr "y + 1")
                                 ]
                                 (edgeEffectAE (sParseEdge "P3 x = y + 1; P4")
                                               [(Just (p "P1", p "P2"), sParseExpr "y + 1")
                                               ])
        , TestCase $ assertEqSet "compute already available expr with same point 1"
                                 [ (Just (p "P1", p "P2"), sParseExpr "y + 1")
                                 ]
                                 (edgeEffectAE (sParseEdge "P3 x = y + 1; P4")
                                               [ (Just (p "P1", p "P2"), sParseExpr "y + 1")
                                               , (Just (p "P3", p "P4"), sParseExpr "y + 1")
                                               ])
        , TestCase $ assertEqSet "compute already available expr with same point 2"
                                 [ (Just (p "P1", p "P2"), sParseExpr "y + 1")
                                 ]
                                 (edgeEffectAE (sParseEdge "P3 x = y + 1; P4")
                                               [ (Just (p "P3", p "P4"), sParseExpr "y + 1")
                                               , (Just (p "P1", p "P2"), sParseExpr "y + 1")
                                               ])
        , TestCase $ assertEqSet "compute already available expr with unknown point"
                                 [ (Just (p "P3", p "P4"), sParseExpr "y + 1")
                                 ]
                                 (edgeEffectAE (sParseEdge "P3 x = y + 1; P4")
                                               [(Nothing, sParseExpr "y + 1")
                                               ])
        , TestCase $ assertEqSet "assign on empty set of AE"
                                 [ (Just (p "START", p "END"), sParseExpr "y + 1")
                                 ]
                                 (edgeEffectAE (sParseEdge "START x = y + 1; END")
                                               [])
        , TestCase $ assertEqSet "assign new expr to new var"
                                 [ (Just (p "P1", p "P2"), sParseExpr "z + 5")
                                 , (Just (p "P2", p "P3"), sParseExpr "y + 3")
                                 ]
                                 (edgeEffectAE (sParseEdge "P2 x = y + 3; P3")
                                               [(Just (p "P1", p "P2"), sParseExpr "z + 5")])
        , TestCase $ assertEqSet "assign new expr to used var"
                                 [ (Just (p "P1", p "P2"), sParseExpr "z + 5")
                                 , (Just (p "P3", p "P4"), sParseExpr "y + 5")
                                 , (Just (p "P4", p "P5"), sParseExpr "y + 10")
                                 ]
                                 (edgeEffectAE (sParseEdge "P4 x = y + 10; P5")
                                               [ (Just (p "P1", p "P2"), sParseExpr "z + 5")
                                               , (Just (p "P6", p "P7"), sParseExpr "x + y")
                                               , (Just (p "P2", p "P3"), sParseExpr "x + y")
                                               , (Just (p "P3", p "P4"), sParseExpr "y + 5")
                                               ])
        , TestCase $ assertEqSet "update variable"
                                 [ (Just (p "P1", p "P2"), sParseExpr "z + 5") ]
                                 (edgeEffectAE (sParseEdge "P4 y = y + 10; P5")
                                               [ (Just (p "P1", p "P2"), sParseExpr "z + 5")
                                               , (Just (p "P2", p "P3"), sParseExpr "x + y")
                                               , (Just (p "P3", p "P4"), sParseExpr "y + 5")
                                               ])
        , TestCase $ assertEqSet "multiple expr"
                                 [ (Just (p "P1", p "P2"), sParseExpr "z + 5")
                                 , (Just (p "P4", p "P5"), sParseExpr "z > 3")
                                 ]
                                 (edgeEffectAE (sParseEdge "P4 Pos(z > 3) P5")
                                               [ (Just (p "P1", p "P2"), sParseExpr "z + 5") ])
        , TestCase $ assertEqSet "load new expr to used var"
                                 [ (Just (p "P1", p "P2"), sParseExpr "z + 5")
                                 , (Just (p "P3", p "P4"), sParseExpr "y + 5")
                                 , (Just (p "P4", p "P5"), sParseExpr "y + 10")
                                 ]
                                 (edgeEffectAE (sParseEdge "P4 x = M[y + 10]; P5")
                                               [ (Just (p "P1", p "P2"), sParseExpr "z + 5")
                                               , (Just (p "P2", p "P3"), sParseExpr "x + y")
                                               , (Just (p "P3", p "P4"), sParseExpr "y + 5")
                                               ])
        ]

      , TestLabel "AvailableExpressions smiley_intersection" $ TestList
        [ TestCase $ assertEqSet "no common exprs"
                                 []
                                 (smiley_intersection [ (Just (p "P1", p "P2"), sParseExpr "x + 5")
                                                      ]
                                                      [ (Just (p "P5", p "p6"), sParseExpr "y + 5")
                                                      ])
        , TestCase $ assertEqSet "one common expr"
                                 [ (Just (p "P1", p "P2"), sParseExpr "x + 5")
                                 , (Just (p "P5", p "P6"), sParseExpr "x + 5")
                                 ]
                                 (smiley_intersection [ (Just (p "P1", p "P2"), sParseExpr "x + 5")
                                                      , (Just (p "P2", p "P3"), sParseExpr "z + 5")
                                                      ]
                                                      [ (Just (p "P5", p "P6"), sParseExpr "x + 5")
                                                      , (Just (p "P4", p "P7"), sParseExpr "x + y")
                                                      ])
        , TestCase $ assertEqSet "several common expr"
                                 [ (Just (p "P1", p "P2"), sParseExpr "x + 5")
                                 , (Just (p "P1", p "P4"), sParseExpr "x + 5")
                                 , (Just (p "P1", p "P9"), sParseExpr "x + 5")
                                 , (Just (p "P5", p "P6"), sParseExpr "x + 5")
                                 , (Just (p "P2", p "P8"), sParseExpr "x + 5")
                                 , (Just (p "P7", p "P6"), sParseExpr "z + 5")
                                 , (Just (p "P2", p "P3"), sParseExpr "z + 5")
                                 ]
                                 (smiley_intersection [ (Just (p "P1", p "P2"), sParseExpr "x + 5")
                                                      , (Just (p "P1", p "P4"), sParseExpr "x + 5")
                                                      , (Just (p "P2", p "P3"), sParseExpr "z + 5")
                                                      ]
                                                      [ (Just (p "P5", p "P6"), sParseExpr "x + 5")
                                                      , (Just (p "P1", p "P9"), sParseExpr "x + 5")
                                                      , (Just (p "P2", p "P8"), sParseExpr "x + 5")
                                                      , (Just (p "P4", p "P7"), sParseExpr "x + y")
                                                      , (Just (p "P7", p "P6"), sParseExpr "z + 5")
                                                      ])
        , TestCase $ assertEqSet "something eats nothing"
                                 [ (Just (p "P1", p "P2"), sParseExpr "x + 5")
                                 ]
                                 (smiley_intersection [ (Just (p "P1", p "P2"), sParseExpr "x + 5")
                                                      ]
                                                      [ (Nothing,               sParseExpr "x + 5")
                                                      ])
        , TestCase $ assertEqSet "2 x nothing == nothing"
                                 [ (Nothing, sParseExpr "x + 5")
                                 ]
                                 (smiley_intersection [ (Nothing, sParseExpr "x + 5")
                                                      ]
                                                      [ (Nothing, sParseExpr "x + 5")
                                                      ])
        ]

      , TestLabel "AvailableExpressions Analysis" $ TestList
        [ TestCase $ assertEqSet "AE prog2"
                                 [ (p "START", [])
                                 , (p "END", [(Just (p "START", p "END"), sParseExpr "y + 1")])
                                 ]
                                 (AvailableExpressions.performAnalysis roundRobin testProg2Parsed)
        ]

      , TestLabel "program eval" $ TestList
        [
        --  TestCase $ assertEqual "eval 1"
        --                         ( [ (p "START", [])
        --                           , (p "END",   [(Nothing, sParseExpr "y + 1")])
        --                           ]
        --                         , False)
        --                         (AnalysisBase.eval
        --                               AvailableExpressions.analysis
        --                               testProg2Parsed
        --                               (p "START")
        --                               (AvailableExpressions.initStateAE testProg2Parsed))
        --                               -- [ (p "START", [((p "START", p "END"), sParseExpr "y + 1")])
        --                               -- , (p "END", [((p "START", p "END"), sParseExpr "y + 1")])
        --                               -- ]
        --, TestCase $ assertEqual "eval 2"
        --                         (sort $
        --                           [ (p "START", [])
        --                           , (p "END",   [(Just (p "START", p "END"), sParseExpr "y + 1")])
        --                           ]
        --                         , False)
        --                         (sort $
        --                          AnalysisBase.eval
        --                               AvailableExpressions.analysis
        --                               testProg2Parsed
        --                               (p "END")
        --                               [ (p "START", [])
        --                               , (p "END",   [(Nothing, sParseExpr "y + 1")])
        --                               ])
          TestCase $ assertEqual "depEdges"
                                 [ (sParseEdge "START x = y + 1; END", p "START")
                                 ]
                                 (evalDependantEdges testProg2Parsed
                                                     (direction AvailableExpressions.analysis)
                                                     (p "END"))
        , TestCase $ assertEqSet "initStateAE"
                                 [ (p "START", [])
                                 , (p "END",   [(Nothing, sParseExpr "y + 1")])
                                 ]
                                 (AvailableExpressions.initStateAE testProg2Parsed)
                                 
        ]

      , TestLabel "program" $ TestList
        [ TestCase $ assertEqSet "sf"
                                 []
                                 (evalDependantEdges testProg1Parsed Forward (Point "START"))
        , TestCase $ assertEqSet "ef"
                                 []
                                 (evalDependantEdges testProg1Parsed Backward (Point "END"))
        , TestCase $ assertEqSet "p1f"
                                 [(sParseEdge "START x = 2; P1", Point "START")]
                                 (evalDependantEdges testProg1Parsed Forward (Point "P1"))
        , TestCase $ assertEqSet "p3f"
                                 [ (sParseEdge "P2 t = x * 21; P3", Point "P2")
                                 , (sParseEdge "P7 ; P3", Point "P7")]
                                 (evalDependantEdges testProg1Parsed Forward (Point "P3"))
        , TestCase $ assertEqSet "p1b"
                                 [(sParseEdge "P1 y = 36; P2", Point "P2")]
                                 (evalDependantEdges testProg1Parsed Backward (Point "P1"))
        , TestCase $ assertEqSet "p3b"
                                 [ (sParseEdge "P3 Pos(y > 0) P4", Point "P4")
                                 , (sParseEdge "P3 Neg(y > 0) P5", Point "P5")]
                                 (evalDependantEdges testProg1Parsed Backward (Point "P3"))
        , TestCase $ assertEqSet "all points"
                                 (map Point $ ["START", "END"] ++ map (("P" ++) . show) [1..8])
                                 (programPoints testProg1Parsed)
        , TestCase $ assertEqSet "all vars"
                                 (map Var ["x", "y", "t"])
                                 (programVars testProg1Parsed)
        ]
      ]

main = runTestTT root

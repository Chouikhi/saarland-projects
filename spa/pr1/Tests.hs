module Tests where

import Test.HUnit

import Data.List (sort)

import TrulyLiveVariables
import AvailableExpressions
import Program
import Parser

sParseExpr = parseExpr . lexer
sParseLabel = parseLabel . lexer
sParseEdge = parseEdge . lexer
sParseProgram = parseProgram . lexer

p = Point

assertEqSet str e1 e2 = assertEqual str (sort e1) (sort e2)

testProg1 = unlines[
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
                                 [ ((p "P1", p "P2"), sParseExpr "x + 3") ]
                                 (edgeEffectAE (sParseEdge "P2 ; P3")
                                               [((p "P1", p "P2"), sParseExpr "x + 3")])
        , TestCase $ assertEqSet "assign new expr to new var"
                                 [ ((p "P1", p "P2"), sParseExpr "z + 5")
                                 , ((p "P2", p "P3"), sParseExpr "y + 3")
                                 ]
                                 (edgeEffectAE (sParseEdge "P2 x = y + 3; P3")
                                               [((p "P1", p "P2"), sParseExpr "z + 5")])
        , TestCase $ assertEqSet "assign new expr to used var"
                                 [ ((p "P1", p "P2"), sParseExpr "z + 5")
                                 , ((p "P3", p "P4"), sParseExpr "y + 5")
                                 , ((p "P4", p "P5"), sParseExpr "y + 10")
                                 ]
                                 (edgeEffectAE (sParseEdge "P4 x = y + 10; P5")
                                               [ ((p "P1", p "P2"), sParseExpr "z + 5")
                                               , ((p "P2", p "P3"), sParseExpr "x + y")
                                               , ((p "P3", p "P4"), sParseExpr "y + 5")
                                               ])
        , TestCase $ assertEqSet "update variable"
                                 [ ((p "P1", p "P2"), sParseExpr "z + 5") ]
                                 (edgeEffectAE (sParseEdge "P4 y = y + 10; P5")
                                               [ ((p "P1", p "P2"), sParseExpr "z + 5")
                                               , ((p "P2", p "P3"), sParseExpr "x + y")
                                               , ((p "P3", p "P4"), sParseExpr "y + 5")
                                               ])
        , TestCase $ assertEqSet "multiple expr"
                                 [ ((p "P1", p "P2"), sParseExpr "z + 5")
                                 , ((p "P4", p "P5"), sParseExpr "z > 3")
                                 ]
                                 (edgeEffectAE (sParseEdge "P4 Pos(z > 3) P5")
                                               [ ((p "P1", p "P2"), sParseExpr "z + 5") ])
        , TestCase $ assertEqSet "load new expr to used var"
                                 [ ((p "P1", p "P2"), sParseExpr "z + 5")
                                 , ((p "P3", p "P4"), sParseExpr "y + 5")
                                 , ((p "P4", p "P5"), sParseExpr "y + 10")
                                 ]
                                 (edgeEffectAE (sParseEdge "P4 x = M[y + 10]; P5")
                                               [ ((p "P1", p "P2"), sParseExpr "z + 5")
                                               , ((p "P2", p "P3"), sParseExpr "x + y")
                                               , ((p "P3", p "P4"), sParseExpr "y + 5")
                                               ])
        ]

      , TestLabel "program" $ TestList
        [ TestCase $ assertEqSet "sf"
                                 []
                                 (evalDependantEdges testProg1Parsed Forward (Point "START"))
        , TestCase $ assertEqSet "ef"
                                 []
                                 (evalDependantEdges testProg1Parsed Backward (Point "END"))
        , TestCase $ assertEqSet "p1f"
                                 [(sParseLabel "x = 2;", Point "START")]
                                 (evalDependantEdges testProg1Parsed Forward (Point "P1"))
        , TestCase $ assertEqSet "p3f"
                                 [ (sParseLabel "t = x * 21;", Point "P2")
                                 , (sParseLabel ";", Point "P7")]
                                 (evalDependantEdges testProg1Parsed Forward (Point "P3"))
        , TestCase $ assertEqSet "p1b"
                                 [(sParseLabel "y = 36;", Point "P2")]
                                 (evalDependantEdges testProg1Parsed Backward (Point "P1"))
        , TestCase $ assertEqSet "p3b"
                                 [ (sParseLabel "Pos(y > 0)", Point "P4")
                                 , (sParseLabel "Neg(y > 0)", Point "P5")]
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

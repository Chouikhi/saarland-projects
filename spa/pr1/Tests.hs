module Tests where

import Test.HUnit

import Data.List (sort)

import TrulyLiveVariables
import Program
import Parser

sParseExpr = parseExpr . lexer
sParseLabel = parseLabel . lexer
sParseProgram = parseProgram . lexer

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
        [ TestCase $ assertEqual "nop" (sParseLabel ";") Nop
        , TestCase $ assertEqual "assign" (sParseLabel "x = y;") (Assign (Var "x") (AExpr $ AtomVar (Var "y")))
        , TestCase $ assertEqual "load" (sParseLabel "x = M[23*y];") (Load (Var "x") (BExpr (AExpr (AtomConst 23))
                                                                                           Times
                                                                                           (AExpr (AtomVar (Var "y")))))
        ]
      , TestLabel "TrulyLiveVariables Edge effects" $ TestList
        [ TestCase (assertEqual "nop" (edgeEffectTLV Nop [(Var "x")]) [(Var "x")])
        , TestCase (assertEqual "assign_to_live"
                                (edgeEffectTLV
                                               (sParseLabel "x = y;")
                                               [Var "x"])
                                [Var "y"])
        , TestCase (assertEqual "assign_to_dead" (edgeEffectTLV (sParseLabel "x = y;")
                                                                [Var "x", Var "y"])
                                                 [Var "y"])
        , TestCase $ assertEqSet "assign_const" (edgeEffectTLV (sParseLabel "x = 5;") [Var "x", Var "t"])
                                                [Var "t"]
        ]
      , TestLabel "TrulyLiveVariables eval" $ TestList
        [
        ]
      , TestLabel "program" $ TestList
        [ TestCase $ assertEqSet  "sf" (evalDependantEdges testProg1Parsed Forward (Point "START"))
                                       []
        , TestCase $ assertEqSet  "ef" (evalDependantEdges testProg1Parsed Backward (Point "END"))
                                       []
        , TestCase $ assertEqSet  "p1f" (evalDependantEdges testProg1Parsed Forward (Point "P1"))
                                        [(sParseLabel "x = 2;", Point "START")]
        , TestCase $ assertEqSet  "p3f" (evalDependantEdges testProg1Parsed Forward (Point "P3"))
                                        [ (sParseLabel "t = x * 21;", Point "P2")
                                        , (sParseLabel ";", Point "P7")]
        , TestCase $ assertEqSet  "p1b" (evalDependantEdges testProg1Parsed Backward (Point "P1"))
                                        [(sParseLabel "y = 36;", Point "P2")]
        , TestCase $ assertEqSet  "p3b" (evalDependantEdges testProg1Parsed Backward (Point "P3"))
                                        [ (sParseLabel "Pos(y > 0)", Point "P4")
                                        , (sParseLabel "Neg(y > 0)", Point "P5")]
        , TestCase $ assertEqSet  "all points" (programPoints testProg1Parsed)
                                               (map Point $ ["START", "END"] ++ map (("P" ++) . show) [1..8])
        , TestCase $ assertEqSet  "all vars" (programVars testProg1Parsed)
                                             (map Var ["x", "y", "t"])
        ]
      ]

main = runTestTT root

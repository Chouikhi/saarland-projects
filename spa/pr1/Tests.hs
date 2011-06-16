module Tests where

import Test.HUnit

import Data.List (sort)

import TrulyLiveVariables
import AnalysisBase
import IntervalAnalysis
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

msort a = maybe Nothing (Just . sort) a

testProg2Parsed = sParseProgram testProg2


-- testProg1NTExprs = map sParseExpr [ "x * 21", "y > 0", "y - 1", "x + t" ]


root = TestList
      [ TestLabel "Lexer" $ TestList
        [ TestCase $ assertEqual "lexer x == y"
                                 [TokenStr "x", TokenOp "==", TokenStr "y"]
                                 (lexer "x == y")
        , TestCase $ assertEqual "lexer x != y"
                                 [TokenStr "x", TokenOp "!=", TokenStr "y"]
                                 (lexer "x != y")
        ]
      , TestLabel "Parser" $ TestList
        [ TestCase $ assertEqual "nop" Nop (sParseLabel ";")
        , TestCase $ assertEqual "assign"
                                 (Assign (Var "x") (AExpr $ AtomVar (Var "y")))
                                 (sParseLabel "x = y;")
        , TestCase $ assertEqual "load"
                                 (Load (Var "x") (BExpr (AExpr (AtomConst 23))
                                                        Times
                                                        (AExpr (AtomVar (Var "y")))))
                                 (sParseLabel "x = M[23*y];") 
        , TestCase $ assertEqual "Pos(i >= 0)"
                                 (Pos (BExpr (AExpr (AtomVar (Var "i")))
                                             GreaterEqual
                                             (AExpr (AtomConst 0))))
                                 (sParseLabel "Pos(i >= 0)")
        , TestCase $ assertEqual "Neg(x < 42)"
                                 (Neg (BExpr (AExpr (AtomVar (Var "x")))
                                             LessThan
                                             (AExpr (AtomConst 42))))
                                 (sParseLabel "Neg(x < 42)")
        , TestCase $ assertEqual "== expr"
                                 (BExpr (AExpr (AtomVar (Var "x")))
                                        Equal 
                                        (AExpr (AtomVar (Var "y"))))
                                 (sParseExpr "x == y")
        , TestCase $ assertEqual "edge x = y + z"
                                 (Edge (Point "P1")
                                       (Assign (Var "x")
                                               (BExpr (AExpr (AtomVar (Var "y")))
                                                      Plus
                                                      (AExpr (AtomVar (Var "z")))))
                                       (Point "P2"))
                                 (sParseEdge "P1 x = y + z; P2")
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
      , TestLabel "Interval Analysis interval operations" $ TestList
        [ TestCase $ assertEqual "lub disjoint"
                                 (finInt (-10) 10)
                                 (intLub (finInt (-10) (-5))
                                         (finInt 3   10))
        , TestCase $ assertEqual "lub overlap"
                                 (finInt 3 10)
                                 (intLub (finInt 3 7)
                                         (finInt 5 10))
        , TestCase $ assertEqual "lub contained"
                                 (finInt 3 10)
                                 (intLub (finInt 3 10)
                                         (finInt 4 8))
        , TestCase $ assertEqual "glb overlap"
                                 (finInt 3 10)
                                 (intGlb (finInt (-5) 10)
                                         (finInt 3 15))
        , TestCase $ assertEqual "glb contained"
                                 (finInt 4 8)
                                 (intGlb (finInt 3 10)
                                         (finInt 4 8))
        , TestCase $ assertEqual "unary +"
                                 (finInt 4 8)
                                 (applyUOp UPlus (finInt 4 8))
        , TestCase $ assertEqual "unary -"
                                 (finInt (-4) 8)
                                 (applyUOp UMinus (finInt (-8) 4))
        , TestCase $ assertEqual "binary + finite"
                                 (finInt (-10) 11)
                                 (applyBOp Plus (finInt (-8) 4)
                                                (finInt (-2) 7))
        , TestCase $ assertEqual "binary + infinite"
                                 (leInt  11)
                                 (applyBOp Plus (leInt 4)
                                                (finInt (-2) 7))
        , TestCase $ assertEqual "binary - finite"
                                 (finInt (-10) (-7))
                                 (applyBOp Minus (finInt (-3) (-1))
                                                 (finInt 6 7))
        , TestCase $ assertEqual "binary - infinite"
                                 fullInt
                                 (applyBOp Minus (leInt 5)
                                                 (leInt (-3)))
        , TestCase $ assertEqual "binary * lecture example 1"
                                 (finInt 0 8)
                                 (applyBOp Times (finInt 0 2)
                                                 (finInt 3 4))
        , TestCase $ assertEqual "binary * lecture example 2"
                                 (finInt (-4) 8)
                                 (applyBOp Times (finInt (-1) 2)
                                                 (finInt 3 4))
        , TestCase $ assertEqual "binary * lecture example 3"
                                 (finInt (-6) 8)
                                 (applyBOp Times (finInt (-1) 2)
                                                 (finInt (-3) 4))
        , TestCase $ assertEqual "binary * lecture example 4"
                                 (finInt (-8) 4)
                                 (applyBOp Times (finInt (-1) 2)
                                                 (finInt (-4) (-3)))
        , TestCase $ assertEqual "binary * infinity 1"
                                 fullInt
                                 (applyBOp Times (leInt 3)
                                                 (geInt (-3)))
        , TestCase $ assertEqual "binary * infinity 2"
                                 (geInt 15)
                                 (applyBOp Times (geInt 3)
                                                 (geInt 5))
        , TestCase $ assertEqual "binary / ex 1"
                                 (finInt 2 12)
                                 (applyBOp Div (finInt 10 25)
                                               (finInt 2 5))
        , TestCase $ assertEqual "binary / ex 2"
                                 (finInt 2 12)
                                 (applyBOp Div (finInt (-10) (-25))
                                               (finInt (-2) (-5)))
        , TestCase $ assertEqual "binary / ex 3"
                                 (finInt (-13) (-2))  -- (-25) / 2 is -13
                                 (applyBOp Div (finInt (-10) (-25))
                                               (finInt (2) (5)))
        , TestCase $ assertEqual "binary / ex 4 (has 0)"
                                 fullInt
                                 (applyBOp Div (finInt (-10) (25))
                                               (finInt (-2) (-5)))
        , TestCase $ assertEqual "binary / ex 5"
                                 (leInt (-5))
                                 (applyBOp Div (leInt (-25))
                                               (finInt 2 5))
        , TestCase $ assertEqual "eval expr var"
                                 (finInt 5 10)
                                 (evalExpr (sParseExpr "x")
                                           ( [(Var "x", finInt 5 10)]))
        , TestCase $ assertEqual "eval expr const"
                                 (finInt 5 5)
                                 (evalExpr (sParseExpr "5")
                                           ( [(Var "x", finInt 5 10)]))
        , TestCase $ assertEqual "eval expr unary"
                                 (finInt (-10) (-5))
                                 (evalExpr (sParseExpr "-x")
                                           ( [(Var "x", finInt 5 10)]))
        , TestCase $ assertEqual "eval expr binary"
                                 (finInt 10 30)
                                 (evalExpr (sParseExpr "x * y")
                                           ( [ (Var "x", finInt 5 10)
                                             , (Var "y", finInt 2 3)
                                             ]))
        , TestCase $ assertEqual "eval expr binary <"
                                 iTrue
                                 (evalExpr (sParseExpr "x < y")
                                           ( [ (Var "x", finInt (-3) 5)
                                             , (Var "y", finInt 6 19)
                                             ]))
        , TestCase $ assertEqual "eval expr binary <"
                                 iUnknown
                                 (evalExpr (sParseExpr "x < y")
                                           ( [ (Var "x", finInt (-3) 5)
                                             , (Var "y", finInt 5 19)
                                             ]))
        , TestCase $ assertEqual "eval expr binary <"
                                 iFalse
                                 (evalExpr (sParseExpr "x < y")
                                           ( [ (Var "x", finInt 5 10)
                                             , (Var "y", finInt (-3) 5)
                                             ]))
        , TestCase $ assertEqual "eval expr binary =="
                                 iTrue
                                 (evalExpr (sParseExpr "x == y")
                                           ( [ (Var "x", finInt 5 5)
                                             , (Var "y", finInt 5 5)
                                             ]))
        , TestCase $ assertEqual "eval expr binary =="
                                 iUnknown
                                 (evalExpr (sParseExpr "x == y")
                                           ( [ (Var "x", finInt 5 6)
                                             , (Var "y", finInt 5 6)
                                             ]))
        , TestCase $ assertEqual "eval expr binary =="
                                 iFalse
                                 (evalExpr (sParseExpr "x == y")
                                           ( [ (Var "x", finInt 5 6)
                                                 , (Var "y", finInt 7 8)
                                                 ]))
        , TestCase $ assertEqual "+ int"
                                 (Just [ (Var "x", finInt 5 10)
                                       ])
                                 (edgeEffectIA (sParseLabel "x = x + 1;")
                                               (Just [ (Var "x", finInt 4 9)
                                                     ]))
        , TestCase $ assertEqual "evalExr x < 42"
                                 iTrue
                                 (evalExpr (sParseExpr "x < 42")
                                           ( [ (Var "x", finInt 0 41) ]))
        -- , TestCase $ assertEqual "Neg(x < 42) for x in [0, 41]"
        --                          (botIA)
        --                          (edgeEffectIA (sParseLabel "Neg(x < 42)")
        --                                        (Just [ (Var "x", finInt 0 41) ]))
        , TestCase $ assertEqual "exploiting Pos(x < 42)"
                                 (Just [ (Var "x", finInt 0 41) ])
                                 (edgeEffectIA (sParseLabel "Pos(x < 42)")
                                               (Just [ (Var "x", finInt 0 42) ]))
        , TestCase $ assertEqual "maybeTrue"
                                 False
                                 (maybeTrue iFalse)
        , TestCase $ assertEqual "maybeTrue"
                                 True
                                 (maybeTrue iTrue)
        , TestCase $ assertEqual "maybeTrue"
                                 True
                                 (maybeTrue iUnknown)
        , TestCase $ assertEqual "maybeFalse"
                                 False
                                 (maybeFalse iTrue)
        , TestCase $ assertEqual "edgeEffectIA Neg(x < 42)"
                                 botIA
                                 (edgeEffectIA (sParseLabel "Neg(x < 42)")
                                               (Just [ (Var "x", finInt 0 41) ]))
        , TestCase $ assertEqual "edge effect Neg(x >= 0) for x = [0, 41]"
                                 botIA
                                 (edgeEffectIA (sParseLabel "Neg(x >= 0)")
                                               (Just [ (Var "x", finInt 0 41) ]))
        , TestCase $ assertEqual "edgeEffectIA Pos(x < 42)"
                                 (Just [ (Var "x", finInt 0 41) ])
                                 (edgeEffectIA (sParseLabel "Pos(x < 42)")
                                               (Just [ (Var "x", finInt 0 41) ]))
        , TestCase $ assertEqual "edgeEffectIA Pos(x < y)"
                                 (msort (Just [ (Var "x", finInt 0 6) 
                                              , (Var "y", finInt 3 7)
                                              ]))
                                 (msort (edgeEffectIA (sParseLabel "Pos(x < y)")
                                                      (Just [ (Var "x", finInt 0 10) 
                                                            , (Var "y", finInt 3 7)
                                                            ])))
        , TestCase $ assertEqual "edgeEffectIA Neg(y < x)"
                                 (msort (Just [ (Var "x", finInt 0 7) 
                                              , (Var "y", finInt 3 7)
                                              ]))
                                 (msort (edgeEffectIA (sParseLabel "Neg(y < x)")
                                                      (Just [ (Var "x", finInt 0 10)
                                                            , (Var "y", finInt 3 7)
                                                            ])))
        , TestCase $ assertEqSet "eval Neg(x >= 0) for  x = [0, 41]"
                                 [ (Point "START", Just [ (Var "x", finInt 0 41) ])
                                 , (Point "END", Nothing)
                                 ]
                                 (fst $ (eval narrowingAnalysis
                                              (sParseProgram "START Neg(x >= 0) END")
                                              (Point "END")
                                              ([ (Point "START", Just [ (Var "x", finInt 0 41) ])
                                               , (Point "END", Just [ (Var "x", geInt 0) ])
                                               ])))
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
        -- , TestCase $ assertEqual "labelSub"
        --                          (sParseLabel "x = 3 + z;")
        --                          (labelSub (sParseLabel "x = y + z;")
        --                                    [ (Var "y", 3)
        --                                    , (Var "v", 100)
        --                                    ])
        ]
      ]

main = runTestTT root

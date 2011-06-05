{

module Parser where

import Char (isSpace, isAlpha, isDigit)
import List (isPrefixOf, find)
import qualified Program

}

%name parseAnalysis AnalysisData
%name parseProgram Program
%name parseLabel Label
%name parseEdge Edge
%name parseExpr Expr
%tokentype { Token }
%error { parseError }

%token 
      'ANALYSIS'      { TokenAnalysis }
      'ALGORITHM'     { TokenAlgorithm }
      'OUTPUT'        { TokenOutput }
      'PROGRAM'       { TokenProgram }

      str             { TokenStr $$ }
      int             { TokenInt $$ }

      '='             { TokenAssign }

      'Pos'           { TokenPos }
      'Neg'           { TokenNeg }

      op              { TokenOp $$ }

      '('             { TokenOB }
      ')'             { TokenCB }
      
      'M'             { TokenMem }
      '['             { TokenOSB }
      ']'             { TokenCSB }
      ';'             { TokenSc }

%%

AnalysisData
    : 'ANALYSIS' str
      'ALGORITHM' str
      'OUTPUT' str
      'PROGRAM' Program            { AnalysisData { analysis  = Analysis $2
                                                  , algorithm = Algorithm $4
                                                  , output    = Output $6
                                                  , program   = reverse $8 } }

-- NOTE: the program edges are stored in reverse order in Program
Program
    : Edge                         { [$1] }
    | Program Edge                 { $2 : $1 }

Edge
    : Point Label Point            { Program.Edge { Program.start = $1
                                                  , Program.label = $2
                                                  , Program.end   = $3
                                                  } }

Point
    : str                          { Program.Point $1 }

Label
    : ';'                            { Program.Nop }
    | str '=' Expr ';'               { Program.Assign (Program.Var $1) $3 }
    | 'Pos' '(' Expr ')'             { Program.Pos $3 }
    | 'Neg' '(' Expr ')'             { Program.Pos $3 }
    | str '=' 'M' '[' Expr ']' ';'   { Program.Load (Program.Var $1) $5 }
    | 'M' '[' Expr ']' '=' Expr ';'  { Program.Store $3 $6 }

Expr
    : str                          { Program.AExpr $ Program.AtomVar (Program.Var $1) }
    | int                          { Program.AExpr $ Program.AtomConst $1 }
    | op Expr                      { Program.UExpr (read $1) $2 }
    | Expr op Expr                 { Program.BExpr $1 (read $2) $3 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

newtype Analysis = Analysis String deriving Show
newtype Algorithm = Algorithm String deriving Show
newtype Output = Output String deriving Show

data AnalysisData = AnalysisData
        { analysis :: Analysis
        , algorithm :: Algorithm
        , output :: Output
        , program :: Program.Program }
      deriving Show

data Token
      = TokenAnalysis
      | TokenAlgorithm
      | TokenOutput
      | TokenProgram

      | TokenStr String
      | TokenInt Int
      | TokenOp String

      | TokenAssign

      | TokenPos
      | TokenNeg

      | TokenOB
      | TokenCB

      | TokenMem
      | TokenOSB
      | TokenCSB
      | TokenSc
      deriving (Show, Eq)

lexer [] = []
lexer (css@(c:cs))
      | isSpace c = lexer cs
      | isDigit c = TokenInt (read num) : lexer restn
      | something op_pair   = case op_pair of Just ops -> TokenOp ops : lexer (drop (length ops) css)
      | something fixed_str = case fixed_str of Just (fixed, tok) -> tok : lexer (drop (length fixed) css)
      | otherwise = TokenStr word : lexer rest
  where
      fixed_str = find (\(fixed, tok) -> fixed `isPrefixOf` css) fixed_strs
      op_pair = find (\op -> op `isPrefixOf` css) ops

      fixed_strs = [ ("ANALYSIS", TokenAnalysis)
                   , ("ALGORITHM", TokenAlgorithm)
                   , ("OUTPUT", TokenOutput)
                   , ("PROGRAM", TokenProgram)
                   , ("Pos", TokenPos)
                   , ("Neg", TokenNeg)
                   , ("M", TokenMem)
                   , ("=", TokenAssign)
                   , ("(", TokenOB)
                   , (")", TokenCB)
                   , ("[", TokenOSB)
                   , ("]", TokenCSB)
                   , (";", TokenSc)
                   ]
      ops = [ "+", "-", "*", "/"
            , "<", ">" , "<=", ">=", "==", "!=" ]

      (num, restn) = span isDigit css
      (word, rest) = span isIdentifierChar css

      isIdentifierChar c = isAlpha c || isDigit c || c == '_'
      something maybe = case maybe of
                             Just z -> True
                             Nothing -> False

-- main = getContents >>= print . calc . lexer
-- main = getContents >>= print . lexer

}

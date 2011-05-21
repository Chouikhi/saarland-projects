{

module Main where
import Char (isSpace, isAlpha, isDigit)
import List (isPrefixOf)
import qualified Program

}

%name calc
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
--      '<'             { TokenLT }
--      '>'             { TokenGT }
--      '<='            { TokenLE }
--      '>='            { TokenGE }
--      '=='            { TokenEq }
--      '!='            { TokenNeq }
--
--      '+'             { TokenPlus }
--      '-'             { TokenMinus }
--      '*'             { TokenTimes }
--      '/'             { TokenDiv }

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

      | TokenAssign

      | TokenPos
      | TokenNeg
      | TokenOp String
--       | TokenLT
--       | TokenGT
--       | TokenLE
--       | TokenGE
--       | TokenEq
--       | TokenNeq
-- 
--       | TokenPlus
--       | TokenMinus
--       | TokenTimes
--       | TokenDiv

      | TokenOB
      | TokenCB

      | TokenMem
      | TokenOSB
      | TokenCSB
      | TokenSc
      deriving Show

-- lexer :: String -> [Token]
-- lexer [] = []
-- lexer (css@(c:cs))
--       | isSpace c = lexer cs
--       | isAlpha c = lexStr css
--       | isDigit c = lexNum css
--       | "==" `isPrefixOf` css = TokenEq  : lexer (drop 2 css)
--       | "!=" `isPrefixOf` css = TokenNeq : lexer (drop 2 css)
--       | "<=" `isPrefixOf` css = TokenLE  : lexer (drop 2 css)
--       | ">=" `isPrefixOf` css = TokenGE  : lexer (drop 2 css)
-- 
-- lexer ('<':cs) = TokenLT : lexer cs
-- lexer ('>':cs) = TokenGT : lexer cs
-- lexer ('=':cs) = TokenAssign : lexer cs
-- lexer ('+':cs) = TokenPlus : lexer cs
-- lexer ('-':cs) = TokenMinus : lexer cs
-- lexer ('*':cs) = TokenTimes : lexer cs
-- lexer ('/':cs) = TokenDiv : lexer cs
-- lexer ('(':cs) = TokenOB : lexer cs
-- lexer (')':cs) = TokenCB : lexer cs
-- lexer ('[':cs) = TokenOSB : lexer cs
-- lexer (']':cs) = TokenCSB : lexer cs
-- lexer (';':cs) = TokenSc : lexer cs

lexer :: String -> [Token]
lexer [] = []
lexer (css@(c:cs))
      | isSpace c = lexer cs
      | isAlpha c = lexStr css
      | isDigit c = lexNum css
      | "==" `isPrefixOf` css = TokenOp "=="  : lexer (drop 2 css)
      | "!=" `isPrefixOf` css = TokenOp "!=" : lexer (drop 2 css)
      | "<=" `isPrefixOf` css = TokenOp "<=" : lexer (drop 2 css)
      | ">=" `isPrefixOf` css = TokenOp ">=" : lexer (drop 2 css)

lexer ('<':cs) = TokenOp "<" : lexer cs
lexer ('>':cs) = TokenOp ">" : lexer cs
lexer ('=':cs) = TokenAssign : lexer cs
lexer ('+':cs) = TokenOp "+" : lexer cs
lexer ('-':cs) = TokenOp "-" : lexer cs
lexer ('*':cs) = TokenOp "*" : lexer cs
lexer ('/':cs) = TokenOp "/" : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer ('[':cs) = TokenOSB : lexer cs
lexer (']':cs) = TokenCSB : lexer cs
lexer (';':cs) = TokenSc : lexer cs

lexNum cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexStr cs =
   case span isIdentifierChar cs of
      ("ANALYSIS", rest)    -> TokenAnalysis : lexer rest
      ("ALGORITHM", rest)   -> TokenAlgorithm : lexer rest
      ("OUTPUT", rest)      -> TokenOutput : lexer rest
      ("PROGRAM", rest)     -> TokenProgram : lexer rest
      ("Pos", rest)         -> TokenPos : lexer rest
      ("Neg", rest)         -> TokenNeg : lexer rest
      ("M", rest)           -> TokenMem : lexer rest

      (str, rest)           -> TokenStr str : lexer rest

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlpha c || isDigit c || c == '_'

main = getContents >>= print . calc . lexer
-- main = getContents >>= print . lexer

}

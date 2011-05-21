{
module Main where
import Char (isSpace, isAlpha, isDigit)
import List (isPrefixOf)
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
    : Point Label Point            { ProgramEdge { start = $1, label = $2, end = $3 } }
--     : Point Label Point            { ProgramEdge $1 $2 $3 }

Point
    : str                          { ProgramPoint $1 }

Label
    : ';'                            { Nop }
    | str '=' Expr ';'               { Assign (Var $1) $3 }
    | 'Pos' '(' Expr ')'             { Pos $3 }
    | 'Neg' '(' Expr ')'             { Pos $3 }
    | str '=' 'M' '[' Expr ']' ';'   { Load (Var $1) $5 }
    | 'M' '[' Expr ']' '=' Expr ';'  { Store $3 $6 }

Expr
    : str                          { AExpr $ AtomVar (Var $1) }
    | int                          { AExpr $ AtomConst $1 }
    | op Expr                      { UExpr (read $1) $2 }
    | Expr op Expr                 { BExpr $1 (read $2) $3 }

-- Exp   : let var '=' Exp in Exp  { Let $2 $4 $6 }
--       | Exp1                    { Exp1 $1 }
-- 
-- Exp1  : Exp1 '+' Term           { Plus $1 $3 }
--       | Exp1 '-' Term           { Minus $1 $3 }
--       | Term                    { Term $1 }
-- 
-- Term  : Term '*' Factor         { Times $1 $3 }
--       | Term '/' Factor         { Div $1 $3 }
--       | Factor                  { Factor $1 }
-- 
-- Factor			  
--       : int                     { Int $1 }
--       | var                     { Var $1 }
--       | '(' Exp ')'             { Brack $2 }
-- 
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
        , program :: Program }
      deriving Show

type Program = [ ProgramEdge ]

data ProgramEdge = ProgramEdge
        { start :: ProgramPoint
        , label :: ProgramLabel
        , end   :: ProgramPoint }
        deriving Show

newtype ProgramPoint = ProgramPoint String deriving (Show, Eq)
data ProgramLabel
        = Nop
        | Pos     ProgramExpr
        | Neg     ProgramExpr
        | Assign  Var ProgramExpr
        | Load    Var ProgramExpr
        | Store   ProgramExpr ProgramExpr
      deriving Show

data ProgramExpr
        = AExpr ProgramAtom
        | UExpr UOp ProgramExpr
        | BExpr ProgramExpr BOp ProgramExpr
      deriving Show

newtype Var = Var String deriving (Show, Eq)

data ProgramAtom
        = AtomVar Var
        | AtomConst Int
      deriving (Show, Eq)

data UOp = UPlus | UMinus deriving (Show, Eq)
data BOp = Plus | Minus | Times | Div
         | Equal | NotEqual
         | LessThan | GreaterThan | LessEqual | GreaterEqual
        deriving (Show, Eq)

instance Read UOp where
  readsPrec _ s = readsUOp s

readsUOp ('+':cs)  = [(UPlus, cs)]
readsUOp ('-':cs)  = [(UMinus, cs)]

instance Read BOp where
  readsPrec _ s = readsBOp s

readsBOp s
      | "<=" `isPrefixOf` s = [(LessEqual, drop 2 s)]
      | ">=" `isPrefixOf` s = [(GreaterEqual, drop 2 s)]
      | "==" `isPrefixOf` s = [(Equal, drop 2 s)]
      | "!=" `isPrefixOf` s = [(NotEqual, drop 2 s)]
readsBOp ('<':cs) = [(LessThan, cs)]
readsBOp ('>':cs) = [(GreaterThan, cs)]
readsBOp ('+':cs) = [(Plus, cs)]
readsBOp ('-':cs) = [(Minus, cs)]
readsBOp ('*':cs) = [(Times, cs)]
readsBOp ('/':cs) = [(Div, cs)]

-- data Exp  
--       = Let String Exp Exp
--       | Exp1 Exp1
--       deriving Show
-- 
-- data Exp1 
--       = Plus Exp1 Term 
--       | Minus Exp1 Term 
--       | Term Term
--       deriving Show
-- 
-- data Term 
--       = Times Term Factor 
--       | Div Term Factor 
--       | Factor Factor
--       deriving Show
-- 
-- data Factor 
--       = Int Int 
--       | Var String 
--       | Brack Exp
--       deriving Show
-- 
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

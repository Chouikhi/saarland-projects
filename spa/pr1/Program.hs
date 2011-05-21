module Program ( Program
               , Edge(..)
               , Point(..)
               , Label(..)
               , Expr(..)
               , Var(..)
               , Atom(..)
               , UOp
               , BOp
               ) where

import Data.List (isPrefixOf, find)

type Program = [ Edge ]

data Edge = Edge
        { start :: Point
        , label :: Label
        , end   :: Point }
        deriving Show

newtype Point = Point String deriving (Show, Eq)
data Label
        = Nop
        | Pos     Expr
        | Neg     Expr
        | Assign  Var Expr
        | Load    Var Expr
        | Store   Expr Expr
      deriving Show

data Expr
        = AExpr Atom
        | UExpr UOp Expr
        | BExpr Expr BOp Expr
      deriving Show

newtype Var = Var String deriving (Show, Eq)

data Atom
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

readsBOp s = case (find prefix_matcher op_map) of
                  Just (ops, op) -> [(op, drop (length ops) s)]
                  Nothing        -> []
    where
      prefix_matcher = (\(ops, _) -> ops `isPrefixOf` s)
      op_map = [ ("<=", LessEqual)
               , (">=", GreaterEqual)
               , ("==", Equal)
               , ("!=", NotEqual)
               , ("<",  LessThan)
               , (">",  GreaterThan)
               , ("+",  Plus)
               , ("-",  Minus)
               , ("*",  Times)
               , ("/",  Div)
               ]
--       | "<=" `isPrefixOf` s = [(LessEqual, drop 2 s)]
--       | ">=" `isPrefixOf` s = [(GreaterEqual, drop 2 s)]
--       | "==" `isPrefixOf` s = [(Equal, drop 2 s)]
--       | "!=" `isPrefixOf` s = [(NotEqual, drop 2 s)]
-- readsBOp ('<':cs) = [(LessThan, cs)]
-- readsBOp ('>':cs) = [(GreaterThan, cs)]
-- readsBOp ('+':cs) = [(Plus, cs)]
-- readsBOp ('-':cs) = [(Minus, cs)]
-- readsBOp ('*':cs) = [(Times, cs)]
-- readsBOp ('/':cs) = [(Div, cs)]

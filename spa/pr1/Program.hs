module Program ( Program
               , Edge(..)
               , PureEdge
               , Point(..)
               , startPoint, endPoint
               , Label(..)
               , Expr(..)
               , Var(..)
               , Atom(..)
               , Direction(..)  -- move this out of here
               , UOp(..)
               , BOp(..)
               , programVars
               , programExprs
               , labelVars
               , isCond
               , evalDependantEdges
               , resultDependantEdges
               , programPoints
               , prettyExpr
               , prettyVar
               , prettyPoint
               , prettyProgram
               , labelExpr
               , labelSub
               , exprVars
               , subExpr
               ) where

import Data.List (isPrefixOf, find, union, sort, nub)
import Data.Maybe (maybeToList, fromJust, isJust)

data Direction = Forward | Backward deriving (Show, Eq)

opposite :: Direction -> Direction
opposite Forward = Backward
opposite Backward = Forward

type Program = [ Edge ]

prettyProgram :: Program -> String
prettyProgram es = unlines $ map prettyEdge (sort es)

programPoints :: Program -> [Point]
programPoints prog = nub . sort $ (map start prog) ++ (map end prog)

programVars :: Program -> [Var]
programVars es = foldr union [] lvs
    where
      lv :: [([Var], Maybe Var)]
      lv = map labelVars $ map label es
      lvs :: [[Var]]
      lvs = map (\(vs, mv) -> vs `union` maybeToList mv) lv

programExprs :: Program -> [Expr]
programExprs es = nub $ sort $ foldr (++) [] $ map (labelExpr . label) es

data Edge = Edge
        { start :: Point
        , label :: Label
        , end   :: Point }
        deriving (Show, Eq, Ord)

prettyEdge :: Edge -> String
prettyEdge (Edge p1 lbl p2) = "    " ++ prettyPoint p1 ++ " "
                                     ++ prettyLabel lbl ++ " "
                                     ++ prettyPoint p2

-- holds only the information about the connected points
type PureEdge = (Point, Point)

newtype Point = Point String deriving (Show, Eq)

instance Ord Point where
  compare (Point p1) (Point p2)
    | p1 == p2                       = EQ
    | p1 == "START" || p2 == "END"   = LT
    | p1 == "END" || p2 == "START"   = GT
    | otherwise                      = compare p1 p2

startPoint = Point "START"
endPoint = Point "END"

prettyPoint :: Point -> String
prettyPoint (Point name) = name

data Label
        = Nop
        | Pos     Expr
        | Neg     Expr
        | Assign  Var Expr
        | Load    Var Expr
        | Store   Expr Expr
  deriving (Show, Eq, Ord)

-- instance Functor Label where
--   fmap _ Nop = Nop
--   fmap f (Pos e) = Pos (fmap f e)
--   fmap f (Neg e) = Neg (fmap f e)
--   fmap f (Assign v e) = Assign

isCond :: Label -> Bool
isCond (Pos _) = True
isCond (Neg _) = True
isCond _ = False

prettyLabel :: Label -> String
prettyLabel Nop = ";"
prettyLabel (Pos e) = "Pos(" ++ prettyExpr e ++ ")"
prettyLabel (Neg e) = "Neg(" ++ prettyExpr e ++ ")"
prettyLabel (Assign v e) = prettyVar v ++ " = " ++ prettyExpr e ++ ";"
prettyLabel (Load v e) = prettyVar v ++ " = M[" ++ prettyExpr e ++ "];"
prettyLabel (Store e1 e2) = "M[" ++ prettyExpr e1 ++ "] = " ++ prettyExpr e2 ++ ";"

labelSub :: Label -> [(Var, Int)] -> Label
labelSub Nop c = Nop
labelSub (Pos e) c = Pos (exprSub e c)
labelSub (Neg e) c = Neg (exprSub e c)
labelSub (Assign v e) c = Assign v (exprSub e c)
labelSub (Load v e) c = Load v (exprSub e c)
labelSub (Store e1 e2) c = Store (exprSub e1 c) (exprSub e2 c)

labelVars :: Label -> ([Var], Maybe Var)
labelVars Nop = ([], Nothing)
labelVars (Pos e) = (exprVars e, Nothing)
labelVars (Neg e) = (exprVars e, Nothing)
labelVars (Assign v e) = (exprVars e, Just v)
labelVars (Load v e) = (exprVars e, Just v)
labelVars (Store e1 e2) = (exprVars e1 `union` exprVars e2, Nothing)

labelExpr :: Label -> [Expr]
labelExpr Nop = []
labelExpr (Pos e) = [e]
labelExpr (Neg e) = [e]
labelExpr (Assign v e) = [e]
labelExpr (Load v e) = [e]
labelExpr (Store e1 e2) = [e1, e2]

evalDependantEdges :: Program -> Direction -> Point -> [(Edge, Point)]
evalDependantEdges prog dir point = map (\e@(Edge u lbl v) -> if dir == Forward then (e, u) else (e, v)) edges
  where
    edges = filter (\(Edge u lbl v) -> if dir == Forward then v == point else u == point) prog

resultDependantEdges :: Program -> Direction -> Point -> [(Edge, Point)]
resultDependantEdges prog dir point = evalDependantEdges prog (opposite dir) point

data Expr
        = AExpr Atom
        | UExpr UOp Expr
        | BExpr Expr BOp Expr
  deriving (Show, Eq, Ord)

exprSub :: Expr -> [(Var, Int)] -> Expr
exprSub a@(AExpr (AtomVar v)) cs = let lu = lookup v cs
                                   in  if isJust lu
                                       then AExpr $ AtomConst $ fromJust lu
                                       else a
exprSub a@(AExpr _) c = a
exprSub (UExpr op e) c = UExpr op (exprSub e c)
exprSub (BExpr e1 op e2) c = BExpr (exprSub e1 c) op (exprSub e2 c)

exprVars :: Expr -> [Var]
exprVars (AExpr (AtomVar v)) = [v]
exprVars (AExpr (AtomConst _)) = []
exprVars (UExpr _ e) = exprVars e
exprVars (BExpr e1 _ e2) = exprVars e1 `union` exprVars e2

subExpr :: Expr -> [Expr]
subExpr ae@(AExpr _) = [ae]
subExpr ue@(UExpr _ e) = ue : subExpr e
subExpr be@(BExpr e1 _ e2) = be : subExpr e1 ++ subExpr e2

prettyExpr :: Expr -> String
prettyExpr (AExpr (AtomVar v)) = prettyVar v
prettyExpr (AExpr (AtomConst c)) = show c
prettyExpr (UExpr uop expr) = prettyUOp uop ++ prettyExpr expr
prettyExpr (BExpr e1 bop e2) = prettyExpr e1 ++ " " ++ prettyBOp bop ++ " " ++ prettyExpr e2

newtype Var = Var String deriving (Show, Eq, Ord)

prettyVar :: Var -> String
prettyVar (Var name) = name

data Atom
        = AtomVar Var
        | AtomConst Int
      deriving (Show, Eq, Ord)

data UOp = UPlus | UMinus deriving (Show, Eq, Ord)
data BOp = Plus | Minus | Times | Div
         | Equal | NotEqual
         | LessThan | GreaterThan | LessEqual | GreaterEqual
        deriving (Show, Eq, Ord)

prettyUOp uop = fromJust $ lookup uop uop2string
  where uop2string = [ (UPlus, "+")
                     , (UMinus, "-")
                     ]
prettyBOp bop = fromJust $ lookup bop bop2string
  where bop2string = [ (Plus, "+")
                     , (Minus, "-")
                     , (Times, "*")
                     , (Div, "/")
                     , (Equal, "==")
                     , (NotEqual, "!=")
                     , (LessThan, "<")
                     , (GreaterThan, ">")
                     , (LessEqual, "<=")
                     , (GreaterEqual, ">=")
                     ]

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

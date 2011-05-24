module AvailableExpressions where

import Program
import Data.Maybe
import Data.List (union)

type CarrierAE = [(Point, Expr)]
type StateAE = [(Point, CarrierAE)]

edgeEffectAE :: Edge -> CarrierAE -> CarrierAE
edgeEffectAE (Edge u lab _) inp = filterVar $ inp `addNewerExpr` ntSubExprsP
  where
    exprs = labelExpr lab
    subExprs = foldr union [] $ map subExpr exprs
    ntSubExprs = filter isNTSubExpr subExprs
    ntSubExprsP = map (\(e) -> (u, e)) ntSubExprs

    isNTSubExpr (AExpr _) = False
    isNTSubExpr _         = True

    alreadyAvailable = map snd inp
    addNewerExpr crnt new = crnt `union` filter (\(_, e) -> e `notElem` alreadyAvailable) new

    (_, mChangedVar) = labelVars lab
    filterVar = if isJust mChangedVar
                then filter (\(p, e) -> fromJust mChangedVar `elem` exprVars e)
                else id

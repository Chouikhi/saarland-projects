module AvailableExpressions where

import Program
import Data.Maybe
import Data.List (union)

type CarrierAE = [(PureEdge, Expr)]
type StateAE = [(Point, CarrierAE)]

edgeEffectAE :: Edge -> CarrierAE -> CarrierAE
edgeEffectAE (Edge u lab v) inp = filterVar $ inp `addNewerExpr` ntSubExprsP
  where
    exprs = labelExpr lab
    subExprs = foldr union [] $ map subExpr exprs
    ntSubExprs = filter isNTSubExpr subExprs
    ntSubExprsP = map (\(e) -> ((u, v), e)) ntSubExprs

    isNTSubExpr (AExpr _) = False
    isNTSubExpr _         = True

    alreadyAvailable = map snd inp
    -- take only expressions which are not already taken
    addNewerExpr crnt new = crnt `union` filter (\(_, e) -> e `notElem` alreadyAvailable) new

    (_, mChangedVar) = labelVars lab
    filterVar = if isJust mChangedVar
                -- remove expressions that have the variable which is written
                then filter (\(_, e) -> fromJust mChangedVar `notElem` exprVars e)
                else id

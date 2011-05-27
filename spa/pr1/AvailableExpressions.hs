{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module AvailableExpressions where

import Program
import FixPointAlgorithmBase
import AnalysisBase
import Data.Maybe
import Data.List (union, intersect, sort, nub, intersperse)

type CarrierAE = [(Maybe PureEdge, Expr)]
type StateAE = State CarrierAE

prettyState :: StateAE -> String
prettyState paes = unlines $ map prettyCarrier paes
  where
    prettyCarrier :: (Point, CarrierAE) -> String
    prettyCarrier (p, aes) = "    " ++ prettyPoint p ++ " : {"
                          ++ (foldr (++) "" (intersperse ", " (allPrettyAes aes)))
                          ++ "}"
    -- allPrettyAes aes = map prettyExpr (nub $ sort $ map snd aes)
    allPrettyAes aes = map prettyAes aes
    prettyAes (Nothing, expr) = "(?) " ++ prettyExpr expr
    prettyAes (Just (p1, p2), expr) = "("
                      ++ prettyPoint p1 ++ ", " ++ prettyPoint p2
                      ++ ") " ++ prettyExpr expr

instance Carrier CarrierAE where

edgeEffectAE :: Edge -> CarrierAE -> CarrierAE
edgeEffectAE (Edge u lab v) inp = filterVar $ inp `addNewerExpr` ntSubExprsP
  where
    exprs = labelExpr lab
    subExprs = nub $ sort $ foldr (++) [] $ map subExpr exprs
    ntSubExprs = filter isNTSubExpr subExprs
    -- ntSubExprsP - all expressions at this edge, with their edge information attached
    ntSubExprsP :: CarrierAE
    ntSubExprsP = map (\(e) -> (Just (u, v), e)) ntSubExprs

    -- addNewerExpr
    -- -- [ .. (Just a, e1) .. ] [ .. (Just b, e1) .. ] = [ .. (Just a, e1) .. ]
    -- -- [ .. (Nothing, e1) .. ] [ .. (Just, e1) .. ] = [ .. (Just, e1) .. ]
    -- there is never Nothing in the new expression list
    addNewerExpr :: CarrierAE -> CarrierAE -> CarrierAE
    -- remove those expr with unknown point, for which a known point exists
    addNewerExpr old new = (filter (\(pe, e) -> isJust pe || e `notElem` map snd realNew) old) ++ realNew
      where
        realNew :: CarrierAE
        -- remove those new expression, which already appear with a fixed point in old
        realNew = filter (\(_, e) -> e `notElem` (map snd $ filter (isJust . fst) old)) new

    (_, mChangedVar) = labelVars lab
    filterVar = if isJust mChangedVar
                -- remove expressions that have the variable which is written
                then filter (\(_, e) -> fromJust mChangedVar `notElem` exprVars e)
                else id

isNTSubExpr :: Expr -> Bool
isNTSubExpr (AExpr _) = False
isNTSubExpr _         = True

analysis = Analysis
  { combine = smiley_intersection
  , direction = Forward
  , edgeEffect = edgeEffectAE
  }

initStateAE :: Program -> StateAE
initStateAE prog = (startPoint, []) : [(p, allExprs) | p <- programPoints prog, p /= startPoint]
  where
    allExprs = map (\e -> (Nothing, e)) $ nub $ sort $ foldr (++) [] $ map (labelExpr . label) prog
    -- compact = map (\(Edge u lbl v) -> ((u, v), filter isNTSubExpr $ labelExpr lbl)) prog
    -- explode pe [] = []
    -- explode pe (e:es) = (pe, e) : explode pe es
    -- allExprs = foldr (++) [] $ map (uncurry explode) compact
    -- exprs = foldr union [] $ map (\(Edge _ lbl _) -> labelExpr lbl) prog

smiley_intersection :: CarrierAE -> CarrierAE -> CarrierAE
smiley_intersection c1 c2 = nub $ sort $ filteredNothing
  where
    -- TODO: Remove Nothing when there is an expr with something
    uniqExpr c = nub $ sort $ map snd c
    exprs = uniqExpr c1 `intersect` uniqExpr c2
    sthExpr c = uniqExpr $ filter (isJust . fst) c
    sthExprs = sthExpr c1 ++ sthExpr c2
    goodExpr _ expr = expr `elem` exprs
    wNothingUnited = filter (uncurry goodExpr) (c1 ++ c2)
    filteredNothing = filter (\(pe, e) -> isJust pe || e `notElem` sthExprs) wNothingUnited


performAnalysis :: (FixPointAlgorithm CarrierAE) -> Program -> StateAE
performAnalysis fpa prog = fpa analysis prog (initStateAE prog)

performOptimization :: Program -> (StateAE) -> Program
performOptimization = undefined

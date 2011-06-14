{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module AvailableExpressions where

import Program
import FixPointAlgorithmBase
import AnalysisBase
import Data.Maybe
import Data.List (union, intersect, sort, nub, intersperse, (\\))
import Util

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

-- TODO: This is getting hairy. Take a second look / add guards.
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
    -- -- [ .. (Just b, e1) .. ] [ .. (Just b, e1) .. ] = [ .. ]
    --    if the new definition already existed, than it cannot be considered
    --    available (nothing can be available to itself).
    -- there is never Nothing in the new expression list
    addNewerExpr :: CarrierAE -> CarrierAE -> CarrierAE
    -- remove those expr with unknown point, for which a known point exists
    addNewerExpr old new = (oldFilterNothing ++ realNew) \\ remove
      where
        oldFilterNothing = filter (\(pe, e) -> isJust pe || e `notElem` map snd realNew) old
        realNew :: CarrierAE
        -- remove those new expression, which already appear with a fixed point in old
        realNew = filter (\(_, e) -> e `notElem` (map snd $ filter (isJust . fst) old)) new

        filtJust cs = filter (\(pe, _) -> isJust pe) cs
        remove = (nub $ sort $ filtJust oldFilterNothing) `intersect` (nub $ sort $ filtJust new)

    (_, mChangedVar) = labelVars lab
    -- remove expressions that have the variable which is written
    filterVar = maybe id (\cv -> filter (\(_, e) -> cv `notElem` exprVars e)) mChangedVar

isNTSubExpr :: Expr -> Bool
isNTSubExpr (AExpr _) = False
isNTSubExpr _         = True

analysis = Analysis
  { combine = smiley_intersection
  , direction = Forward
  , edgeEffect = edgeEffectAE
  -- TODO: Add check for monotonicity here.
  , fix = curry snd
  }

initStateAE :: Program -> StateAE
initStateAE prog = (startPoint, []) : [(p, allExprs) | p <- programPoints prog, p /= startPoint]
  where
    allExprs = map (\e -> (Nothing, e)) $ nub $ sort $ foldr (++) [] $ map (labelExpr . label) prog

smiley_intersection :: CarrierAE -> CarrierAE -> CarrierAE
smiley_intersection c1 c2 = nub $ sort $ filteredNothing
  where
    uniqExpr c = nub $ sort $ map snd c
    -- all common expressions, regardless of definition point
    exprs = uniqExpr c1 `intersect` uniqExpr c2
    sthExpr c = uniqExpr $ filter (isJust . fst) c
    sthExprs = sthExpr c1 ++ sthExpr c2
    goodExpr _ expr = expr `elem` exprs
    wNothingUnited = filter (uncurry goodExpr) (c1 ++ c2)
    -- filter those nothings, that have corresponding somethings
    filteredNothing = filter (\(pe, e) -> isJust pe || e `notElem` sthExprs) wNothingUnited


performAnalysis :: (FixPointAlgorithm CarrierAE) -> Program -> StateAE
performAnalysis fpa prog = fpa analysis prog (initStateAE prog)

performOptimization :: Program -> StateAE -> Program
performOptimization prog apes = prog3 ++ nedges
  where
    -- choose a unique new variable for each non trivial expression
    expr2uvars :: [(Expr, Var)]
    expr2uvars = let uniq_vars = filter (`notElem` (programVars prog))
                         $ map (Var . ("t_" ++) . show) [0..]
                     es = filter isNTSubExpr $ programExprs prog
                 in zip es uniq_vars
    -- given a label, set of expressions and a mapping to variables replace all
    -- expressions in label from the set with the corresponding variable
    labelExprSubEV :: Label -> [Expr] -> [(Expr, Var)] -> Label
    labelExprSubEV lbl es esm = labelExprSub lbl f where
      f e = if e `elem` es
            then AExpr $ AtomVar $ jLookup e esm
            else e

    -- stage1 modifies the program to use the expression variable whenever it
    -- is available (but not where it becomes available) and tells which
    -- expression locations need to be exploited (i.e not all expressions are
    -- going to be used later so only the used ones will be extractd in an
    -- assignment)
    (prog1, exprs) = foldr stage1 ([], []) prog where
      stage1 :: Edge -> (Program, [(Point, Expr)])
                     -> (Program, [(Point, Expr)])
      stage1 e@(Edge u lbl v) (ps, pes) = (ne:ps, pes `union` npe) where
        -- currenlty available point-expressions -- warning! it can have
        -- duplicates. They get transfered to npe, and it get right-united.
        capes = map (\(a, b) -> (fst . fromJust $ a, b)) $ jLookup u apes
        caes = map snd capes
        nexprs = labelExpr lbl
        -- substitute expressions
        sexprs = filter (`elem` caes) nexprs
        npe = filter ((`elem` sexprs) . snd) capes
        ne = e { label = labelExprSubEV lbl sexprs expr2uvars }

    -- stage2 substitute the available expressions with the corresponding
    -- variables in the place where the expression becomes available
    prog2 = map stage2 prog1 where
      stage2 e@(Edge u lbl v) = e { label = new_lbl } where
        fexprs = map snd $ filter ((u ==) . fst) exprs
        new_lbl = labelExprSubEV lbl fexprs expr2uvars
        
    -- stage3 determines the new program points for the expressions that need
    -- to be added to the program and also information specifying how existing
    -- program points in the program should be renamed
    (exprs2, moves, _) = foldr stage3 ([], moves0, upb_init) exprs where
      moves0 = map (\(u, _) -> (u, u)) exprs
      stage3 :: (Point, Expr)
             -> ( [(PureEdge, Expr)], [(Point, Point)]
                , ([Point], [(Point, [Point])]))
             -> ( [(PureEdge, Expr)], [(Point, Point)]
                , ([Point], [(Point, [Point])]))
      stage3 (u, e) (es,    ms,             upts_st) =
                    (ne:es, aeUpd u u'' ms, nupts_st) where
        u' = jLookup u ms
        (u'', nupts_st) = uniq_pnt_after u upts_st
        ne = ((u', u''), e)
      upb_init = ([], [])
      -- given a point return a unique point with similar name. The second arg
      -- is the state, which should be looked after by the caller. It manages
      -- an infinite sequence of similar point names for each program point and
      -- a set of returned points, to ensure uniqueness.
      uniq_pnt_after :: Point -> ([Point], [(Point, [Point])])
                     -> (Point,  ([Point], [(Point, [Point])]))
      uniq_pnt_after u cs@(ret, fut) = (r, (r:ret, aeUpd u rseq nfut)) where
        cseq = lookup u fut
        nfut = if isNothing cseq then add fut u else fut
        oseq = jLookup u nfut
        r:rseq = dropWhile pnt_is_existing oseq
        pnt_is_existing p = p `elem` ret || p `elem` programPoints prog
        add fut pp = (pp, new_seq pp):fut
        new_seq (Point pn) = map (Point . ((pn ++ "_") ++) . show) [0..]

    -- stage4 changes existing program points according to information computed
    -- in stage3
    prog3 = map stage4 prog2 where
      stage4 e@(Edge u _ v) = maybe e (\u' -> e { start = u' }) $ lookup u moves

    -- convert expressions to actual edges -- we have point information so just
    -- create appropriate labels
    nedges = map pnt_expr_to_edge exprs2 where
      pnt_expr_to_edge ((u, v), e) = Edge u (Assign (jLookup e expr2uvars) e) v

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
    -- compact = map (\(Edge u lbl v) -> ((u, v), filter isNTSubExpr $ labelExpr lbl)) prog
    -- explode pe [] = []
    -- explode pe (e:es) = (pe, e) : explode pe es
    -- allExprs = foldr (++) [] $ map (uncurry explode) compact
    -- exprs = foldr union [] $ map (\(Edge _ lbl _) -> labelExpr lbl) prog

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

-- TODO: Use this function for labelSub
-- TODO: Rename X ending functions
-- Given an expression and a function that maps expressions return the resulting expression
exprSubX :: Expr -> (Expr -> Expr) -> Expr
exprSubX e f = if ne /= e
               then ne
               else se 
  where 
    ne = f e
    se = case e of
      (AExpr _) -> e
      (UExpr op ex) -> UExpr op (exprSubX ex f)
      (BExpr ex1 op ex2) -> BExpr (exprSubX ex1 f) op (exprSubX ex2 f)

-- Given a label and a function that maps expressions return the resulting label
labelExprSubX :: Label -> (Expr -> Expr) -> Label
labelExprSubX Nop _ = Nop
labelExprSubX (Pos e) f = Pos (exprSubX e f)
labelExprSubX (Neg e) f = Neg (exprSubX e f)
labelExprSubX (Assign v e) f = Assign v (exprSubX e f)
labelExprSubX (Load v e) f = Load v (exprSubX e f)
labelExprSubX (Store e1 e2) f = Store (exprSubX e1 f) (exprSubX e2 f)

labelExprSub :: Label -> [Expr] -> [(Expr, Var)] -> Label
labelExprSub lbl es esm = labelExprSubX lbl f
  where
    f :: Expr -> Expr
    f e = if e `elem` es
          then AExpr $ AtomVar $ jLookup e esm
          else e

-- TODO: Add nesting to make code clearer.
-- XXX: This does NOT work with Pos/Neg.
performOptimization :: Program -> StateAE -> Program
performOptimization prog apes = prog2 ++ nedges
  where
    expr2uvars :: [(Expr, Var)]
    expr2uvars = let uniq_vars = filter (`notElem` (programVars prog)) $ map (Var . ("t_" ++) . show) [0..]
                     es = filter isNTSubExpr $ programExprs prog
                 in zip es uniq_vars
    -- uniq_pts = filter (`notElem` (programPoints prog)) $ map (Point . ("TP_" ++) . show) [0..]
    upb_init = ([], [])
    uniq_pnt_between :: PureEdge -> ([Point], [(PureEdge, [Point])]) -> (Point, ([Point], [(PureEdge, [Point])]))
    uniq_pnt_between pe@(u, v) cs@(ret, fut) = (r, (r:ret, aeUpd pe rseq nfut))
      where
        cseq = lookup pe fut
        nfut = if isNothing cseq then add fut pe else fut
        r:rseq = dropWhile (\p -> p `elem` ret || p `elem` programPoints prog) $ jLookup pe nfut
        add fut pe = (pe, new_seq $ fst pe):fut
        new_seq (Point ps) = map (Point . ((ps ++ "_") ++) . show) [0..]
    (prog1, exprs) = foldr stage1 ([], []) prog
    -- stage1 modifies the program to use the expression variable whenever it
    -- is available and tell which expression locations need to be exploited
    -- (i.e not all expressions are going to be used later so only the used
    -- ones will be extractd in an assignment)
    stage1 :: Edge -> (Program, [(PureEdge, Expr)]) -> (Program, [(PureEdge, Expr)])
    stage1 e@(Edge u lbl v) (ps, pes) = (ne:ps, npe `union` pes)
      where
        -- currenlty available point-expressions
        capes = map (\(a, b) -> (fromJust a, b)) $ jLookup u apes
        caes = map snd capes
        nexprs = labelExpr lbl
        -- substitute expressions
        sexprs = filter (`elem` caes) nexprs
        npe = filter ((`elem` sexprs) . snd) capes
        ne = e { label = labelExprSub lbl sexprs expr2uvars }
    (exprs2, moves, _) = foldr stage2 ([], moves0, upb_init) exprs
    moves0 = map (\((u, v), _) -> ((u, v), u)) exprs
    stage2 :: (PureEdge, Expr) -> ([(PureEdge, Expr)], [((PureEdge), Point)], ([Point], [(PureEdge, [Point])]))
           -> ([(PureEdge, Expr)], [((PureEdge), Point)], ([Point], [(PureEdge, [Point])]))
    stage2 (pe@(u, v), e) (es, ms, upts_st) = (ne:es, aeUpd pe u'' ms, nupts_st)
      where
        u' = jLookup pe ms
        (u'', nupts_st) = uniq_pnt_between pe upts_st
        ne = ((u', u''), e)
    prog2 = map stage3 prog1
    stage3 e@(Edge u lbl v) = maybe e (\u' -> e { start = u' }) $ lookup (u, v) moves
    nedges = map (\((u, v), e) -> Edge u (Assign (jLookup e expr2uvars) e) v) exprs2

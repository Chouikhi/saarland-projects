{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module IntervalAnalysis where

import Program
import FixPointAlgorithmBase
import AnalysisBase
import Util
import Data.Maybe
import Data.List (sort, intersperse)
import GHC.Exts (the)

newtype Interval = Interval (Maybe Int, Maybe Int) deriving (Eq, Ord)

instance Show Interval where
  show (Interval (ma, mb)) = "[" ++ sn "-" ma ++ ", " ++ sn "+" mb ++ "]"
    where
      sn str Nothing = str ++ "INF"
      sn str (Just a) = show a

type BareCarrierIA = [(Var, Interval)]
type CarrierIA = Maybe BareCarrierIA
type StateIA = State CarrierIA

instance Carrier CarrierIA where
  pretty = prettyState

prettyState :: StateIA -> String
prettyState paes = unlines $ map prettyCarrier paes
  where
    prettyCarrier :: (Point, CarrierIA) -> String
    prettyCarrier (p, vis) = "    " ++ prettyPoint p ++ " : {"
                          ++ (foldr (++) "" (intersperse ", " (allPrettyVIs vis)))
                          ++ "}"
    allPrettyVIs Nothing = [""]
    allPrettyVIs (Just vis) = map prettyVIs $ sort vis
    prettyVIs (v, int) = "(" ++ prettyVar v ++ ", " ++ show int ++ ")"

botIA = Nothing
topVIA = fullInt
topIA vars = Just $ map (\v -> (v, topVIA)) vars

fromEnds :: [InfinityInt] -> Interval
fromEnds as = Interval (infToNothing zmin, infToNothing zmax)
  where
    zmin = minimum as
    zmax = maximum as
    infToNothing ii = if isInf ii then Nothing else Just (getInt ii)

intToInfInt (Interval (ma, mb)) = [ maybe MinusInfinity AInt ma
                                  , maybe PlusInfinity AInt mb
                                  ]

fullInt = Interval (Nothing, Nothing)
leInt a = Interval (Nothing, Just a)
geInt a = Interval (Just a, Nothing)
finInt a b = Interval (Just a, Just b)

iTrue = finInt 1 1
iFalse = finInt 0 0
iUnknown = finInt 0 1
toIBool :: Interval -> Bool -> Interval
toIBool int b = if b && int == iTrue then iTrue
                else if not b && int == iFalse then iFalse
                else iUnknown
iNot :: Interval -> Interval
iNot int = applyBOp Minus iTrue int
maybeTrue int = let [a1, a2] = intToInfInt int
                in a2 == AInt 1
maybeFalse int = let [a1, a2] = intToInfInt int
                 in a1 == AInt 0

hasZero int = sa <= 0 && 0 <= sb
  where
    ia:ib:[] = intToInfInt int
    sa = getSgn ia
    sb = getSgn ib

intLub :: Interval -> Interval -> Interval
intLub int1@(Interval (ma1, mb1)) int2@(Interval (ma2, mb2)) = Interval (a1, a2)
  where
    a1 = ma1 >>= (\a1 -> ma2 >>= (\a2 -> return $ min a1 a2))
    a2 = mb1 >>= (\b1 -> mb2 >>= (\b2 -> return $ max b1 b2))

intGlbM :: Interval -> Interval -> Maybe Interval
intGlbM int1 int2 = if lb <= ub then Just $ fromEnds [lb, ub] else Nothing
  where
    [a1, b1] = intToInfInt int1
    [a2, b2] = intToInfInt int2
    lb = max a1 a2
    ub = min b1 b2
    
intGlb :: Interval -> Interval -> Interval
intGlb int1 int2 = fromJustX ("no glb: " ++ show (int1, int2)) $ intGlbM int1 int2

intHaveGlb :: Interval -> Interval -> Bool
intHaveGlb int1 int2 = isJust $ intGlbM int1 int2

data InfinityInt = MinusInfinity | AInt Int | PlusInfinity deriving (Show, Eq, Ord)

instance Num InfinityInt where
  ia + ib = if isInf ia then ia else if isInf ib then ib else AInt $ getInt ia + getInt ib
  negate ia = if isInf ia then sgnToInf $ negate $ getSgn ia else AInt $ negate $ getInt ia
  ia - ib = ia + (negate ib)
  fromInteger i = AInt (fromInteger i)
  signum = undefined
  abs = undefined
  (*) = undefined

isInf :: InfinityInt -> Bool
isInf MinusInfinity = True
isInf PlusInfinity = True
isInf _ = False

-- returns only -1 0 1
getSgn :: InfinityInt -> Int
getSgn MinusInfinity = -1
getSgn (AInt i) = if i == 0 then 0 else i `div` (abs i)
getSgn PlusInfinity = 1

sgnToInf :: Int -> InfinityInt
sgnToInf (-1) = MinusInfinity
sgnToInf  0 = AInt 0
sgnToInf  1 = PlusInfinity
sgnToInf  a = error $ "unexpected sign " ++ show a

getInt :: InfinityInt -> Int
getInt (AInt i) = i
getInt _ = error "Cannot extract integer from infinity"

mplus :: Maybe Int -> Maybe Int -> Maybe Int
mplus ma mb = ma >>= (\a -> mb >>= (\b -> return (a + b)))

mtimes :: InfinityInt -> InfinityInt -> InfinityInt
mtimes a b
  | isInf a || isInf b = sgnToInf ((getSgn a) * (getSgn b))
  | otherwise          = AInt $ (getInt a) * (getInt b)

mdiv :: InfinityInt -> InfinityInt -> InfinityInt
mdiv a b
  | isInf a || isInf b = sgnToInf ((getSgn a) * (getSgn b))
  | otherwise          = AInt $ (getInt a) `div` (getInt b)

applyUOp :: UOp -> Interval -> Interval
applyUOp UPlus int = int
applyUOp UMinus (Interval (ma, mb)) = (Interval (neg mb, neg ma))
  where
    neg Nothing = Nothing
    neg (Just a) = Just (-a)

applyBOp :: BOp -> Interval -> Interval -> Interval
applyBOp Plus (Interval (ma1, mb1)) (Interval (ma2, mb2)) =
  Interval (ma1 `mplus` ma2, mb1 `mplus` mb2)
applyBOp Minus int1 int2 =
  applyBOp Plus int1 $ applyUOp UMinus int2
applyBOp Times int1 int2 = fromEnds ends
  where
    ends = [x `mtimes` y | x <- intToInfInt int1, y <- intToInfInt int2]
applyBOp Div int1 int2 = if hasZero int1 || hasZero int2
                         then fullInt
                         else fromEnds ends
  where
    ends = [x `mdiv` y | x <- intToInfInt int1, y <- intToInfInt int2]

applyBOp NotEqual int1 int2 = iNot $ applyBOp Equal int1 int2
applyBOp LessEqual int1 int2 = iNot $ applyBOp LessThan int2 int1
applyBOp GreaterThan int1 int2 = applyBOp LessThan int2 int1
applyBOp GreaterEqual int1 int2 = applyBOp LessEqual int2 int1

applyBOp bop int1 int2 = applyBOpX bop (a1, b1) (a2, b2)
  where
    [a1, b1] = intToInfInt int1
    [a2, b2] = intToInfInt int2

applyBOpX Equal (a1, b1) (a2, b2) = if a1 == b1 && a2 == b2 && a1 == a2 then iTrue
                                    else if b1 < a2 || b2 < a1 then iFalse
                                    else iUnknown
                                    
applyBOpX LessThan (a1, b1) (a2, b2) = if b1 < a2 then iTrue
                                       else if b2 <= a1 then iFalse
                                       else iUnknown

evalExpr :: Expr -> BareCarrierIA -> Interval
evalExpr (AExpr (AtomVar v)) c = jLookup v c
evalExpr (AExpr (AtomConst i)) c = finInt i i
evalExpr (UExpr uop e) mc  = applyUOp uop $ evalExpr e mc
evalExpr (BExpr e1 bop e2) mc = applyBOp bop (evalExpr e1 mc) (evalExpr e2 mc)

edgeEffectIA :: Label -> CarrierIA -> CarrierIA
edgeEffectIA lbl minp = if minp == botIA
                        then botIA
                        else case lbl of
                               Nop -> minp
                               (Pos e) -> if maybeTrue (evalExpr e inp)
                                          then Just $ exploitCond e inp
                                          else botIA
                               (Neg e) -> if maybeFalse (evalExpr e inp)
                                          then Just $ exploitCond (negBExp e) inp
                                          else botIA



                               (Assign v e) -> Just $ aeUpd v (evalExpr e inp) inp
                               (Load v e) -> Just $ aeUpd v topVIA inp
                               (Store _ _) -> Just inp
  where
    inp :: BareCarrierIA
    inp = fromJust minp
    -- TODO: if both sides are variables, exploit for both
    exploitCond :: Expr -> BareCarrierIA -> BareCarrierIA
    exploitCond bexp inp = case bexp of
                              (BExpr (AExpr (AtomVar v)) op ex2)
                                -> aeUpdF v (updV op ex2 inp) inp
                              (BExpr ex1 op (AExpr (AtomVar v)))
                                -> aeUpdF v (updV (revBOp op) ex1 inp) inp
      where
        -- updV op exp inp = intGlb $ condVarIntR op $ evalExpr exp inp
        updV op exp inp int = if intHaveGlb in2 int
                              then intGlb in2 int
                              else error ((prettyExpr bexp) ++ ": no glb " ++ (show in2) ++ " " ++ (show int))
          where in2 = condVarIntR op $ evalExpr exp inp
    condVarIntR :: BOp -> Interval -> Interval
    condVarIntR op int
      | op == Equal = int
      | op == NotEqual = fullInt
      | op == LessEqual = fromEnds [MinusInfinity, ub]
      | op == GreaterEqual = fromEnds [lb, PlusInfinity]
      | op == LessThan = fromEnds [MinusInfinity, ub - 1]
      | op == GreaterThan = fromEnds [lb + 1, PlusInfinity]
      where
        [lb, ub] = intToInfInt int
    negBExp (BExpr e1 op e2) = BExpr e1 (negBOp op) e2
    negBOp op = jLookup op $ mkl [ (Equal, NotEqual)
                                 , (LessThan, GreaterEqual)
                                 , (LessEqual, GreaterThan)
                                 ]
    revBOp op = jLookup op $ mkl [ (Equal, Equal)
                                 , (NotEqual, NotEqual)
                                 , (LessThan, GreaterThan)
                                 , (LessEqual, GreaterEqual)
                                 ]
    mkl = foldr (\p s -> if fst p /= snd p then p:swap p:s else p:s) []
    swap (a, b) = (b, a)

combineIA :: (Interval -> Interval -> Interval)
          -> CarrierIA -> CarrierIA -> CarrierIA
combineIA combineInt c1 c2
  | c1 == botIA = c2
  | c2 == botIA = c1
  | otherwise = let jc1 = fromJust c1
                    jc2 = fromJust c2
                    vars1 = map fst jc1
                    vars2 = map fst jc2
                    vars = if sort vars1 == sort vars2
                           then vars1
                           else error "carriers have different set of variables"
                in Just $ map (\v -> (v, combineInt (jLookup v jc1)
                                                    (jLookup v jc2)))
                              vars

-- fixCarrier :: fix function, what to do with bot input, oldc, newc -> fixed
fixCarrier :: (Interval -> Interval -> Interval) -> Bool
           -> CarrierIA -> CarrierIA -> CarrierIA
fixCarrier fixer up c1 c2
  | c1 == botIA = if up then c2 else botIA
  | c2 == botIA = if up then c1 else botIA
  | otherwise = let cc1 = sort $ fromJust c1
                    cc2 = sort $ fromJust c2
                    zipper (p1, i1) (p2, i2)
                      = if p1 /= p2
                        then error "different variables in widen"
                        else (p1, fixer i1 i2)
                in Just $ zipWith zipper cc1 cc2

widenInt :: Interval -> Interval -> Interval
widenInt i1 i2 = fromEnds [lb, ub]
  where
    [a1, b1] = intToInfInt i1
    [a2, b2] = intToInfInt i2
    lb = if a1 <= a2 then a1 else MinusInfinity
    ub = if b1 >= b2 then b1 else PlusInfinity

narrowInt :: Interval -> Interval -> Interval
narrowInt i1 i2 = fromEnds [lb, ub]
  where
    [a1, b1] = intToInfInt i1
    [a2, b2] = intToInfInt i2
    lb = if isInf a1 then a2 else a1
    ub = if isInf b1 then b2 else b1

stdAnalysis = Analysis
  { combine = combineIA intLub
  , direction = Forward
  , edgeEffect = labelToEdge edgeEffectIA
  , fix = undefined
  }

wideningAnalysis = stdAnalysis { fix = fixCarrier widenInt True }
narrowingAnalysis = stdAnalysis { fix = fixCarrier narrowInt False }

performAnalysis :: (FixPointAlgorithm CarrierIA) -> Program -> StateIA
performAnalysis fpa prog = afterNarrowing
  where
    initState = aeUpd startPoint (topIA $ programVars prog)
                      [(pp, botIA) | pp <- programPoints prog]
    afterWidening = fpa wideningAnalysis prog initState
    afterNarrowing = fpa narrowingAnalysis prog afterWidening

performOptimization :: Program -> StateIA -> Program
performOptimization prog ints = optConst
  where
    egb :: [Edge] -- edges going to bottom
    egb = filter (\(Edge _ _ p2) -> jLookup p2 ints == botIA) prog
    cds :: [Point] -- condition forks to be optimized
    cds = map start $ filter (isCond . label) egb
    isBot p = jLookup p ints == botIA
    optEdges = map (\e@(Edge u _ v) -> if isBot u || isBot v || u `elem` cds
                                       then e { label = Nop }
                                       else e)
                   prog
    optPoints = filter (\(Edge u _ v) -> not (isBot u || isBot v)) optEdges
    optConst = map (\e -> e { label = labelExprSub (label e) (varToConst (consts (start e))) }) optPoints
    varToConst vcs (AExpr (AtomVar v)) = maybe (AExpr (AtomVar v))
                                               (\c -> AExpr (AtomConst c))
                                               (lookup v vcs)
    varToConst vcs e = e
    consts :: Point -> [(Var, Int)]
    consts p = sndMap (getInt . the)
             $ filter (\(_, [lb, ub]) -> lb == ub)
             $ sndMap intToInfInt
             $ maybe [] id
             $ jLookup p ints

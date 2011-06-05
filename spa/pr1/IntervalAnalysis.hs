{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module IntervalAnalysis where

import Program
import FixPointAlgorithmBase
import AnalysisBase
import Data.Maybe

newtype Interval = Interval (Maybe Int, Maybe Int) deriving (Eq)

instance Show Interval where
  show (Interval (ma, mb)) = "[" ++ sn "-" ma ++ ", " ++ sn "+" mb ++ "]"
    where
      sn str Nothing = str ++ "infty"
      sn str (Just a) = show a

type CarrierIA = Maybe [(Var, Interval)]
type StateIA = State CarrierIA

fromEnds :: [InfinityInt] -> Interval
fromEnds as = Interval (infToNothing zmin, infToNothing zmax)
  where
    zmin = minimum as
    zmax = maximum as
    infToNothing ii = if isInf ii then Nothing else Just (getInt ii)

intToInfInt (Interval (ma, mb)) = [ if isJust ma then AInt $ fromJust ma else MinusInfinity
                                  , if isJust mb then AInt $ fromJust mb else PlusInfinity
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

hasZero int = sa <= 0 && 0 <= sb
  where
    ia:ib:[] = intToInfInt int
    sa = getSgn ia
    sb = getSgn ib

intLub :: Interval -> Interval -> Interval
intLub int1@(Interval (ma1, mb1)) int2@(Interval (ma2, mb2)) = Interval (a1, a2)
  where
    a1 = if isJust ma1 && isJust ma2 then Just $ min (fromJust ma1) (fromJust ma2) else Nothing
    a2 = if isJust mb1 && isJust mb2 then Just $ max (fromJust mb1) (fromJust mb2) else Nothing

-- intGlb :: Interval -> Interval -> Interval
-- intGlb int1@(Interval (ma1, mb1)) int2@(Interval (ma2, mb2)) = res
--   where
--     a1 = if isJust ma1 && isJust ma2 then Just $ max (fromJust ma1) (fromJust ma2) else Nothing
--     a2 = if isJust mb1 && isJust mb2 then Just $ min (fromJust mb1) (fromJust mb2) else Nothing
--     res = if isNothing a1 || isNothing a2 || fromJust a1 <= fromJust a2
--           then Interval (a1, a2)
--           else error $ "GLB failed " ++ show int1 ++ " " ++ show int2
intGlbM :: Interval -> Interval -> Maybe Interval
intGlbM int1 int2 = if lb <= ub then Just $ fromEnds [lb, ub] else Nothing
  where
    [a1, b1] = intToInfInt int1
    [a2, b2] = intToInfInt int2
    lb = max a1 a2
    ub = min b1 b2
    
intGlb :: Interval -> Interval -> Interval
intGlb int1 int2 = fromJust $ intGlbM int1 int2

intHaveGlb :: Interval -> Interval -> Bool
intHaveGlb int1 int2 = isJust $ intGlbM int1 int2

data InfinityInt = MinusInfinity | AInt Int | PlusInfinity deriving (Show, Eq, Ord)

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
mplus ma mb = if isJust ma && isJust mb
              then Just $ fromJust ma + fromJust mb
              else Nothing

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

evalExpr :: Expr -> CarrierIA -> Interval
evalExpr _ Nothing = error "evalExpr called with bottom"
evalExpr (AExpr (AtomVar v)) (Just c) = fromJust $ lookup v c
evalExpr (AExpr (AtomConst i)) (Just c) = finInt i i
evalExpr (UExpr uop e) mc  = applyUOp uop $ evalExpr e mc
evalExpr (BExpr e1 bop e2) mc = applyBOp bop (evalExpr e1 mc) (evalExpr e2 mc)

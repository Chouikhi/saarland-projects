{-# LANGUAGE RankNTypes, PolymorphicComponents #-}
module FixPointAlgorithmBase where

import Program
import AnalysisBase

type {- Carrier c => -} FixPointAlgorithm c = (Analysis c) -> Program -> (State c) -> (State c)
data AnyFixPointAlgorithm = WrapFixPointAlgorithm (forall c. Carrier c => FixPointAlgorithm c)

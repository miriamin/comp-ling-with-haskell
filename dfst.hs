-- DFST
-- Greg Kobele
-- 03 Dec 2018 

-- This file provides a module implementing deterministic finite state
-- transducers, qua sequential transducers

{-# LANGUAGE GADTs #-}

module Dfst
  ( SeqTrans
  , ForwardTrans
  , BackwardTrans
  , states
  , inAlph
  , outAlph
  , start
  , initial
  , delta
  , final
  , mkForwardTransducer
  , mkBackwardTransducer
  , transduce
  , compose
  ) where

import Dfsa as A (DFSA(A),recognizes)
-- yes, our very own module
import Data.Foldable as F (foldlM)
-- http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Foldable.html#v:foldlM
import Data.Maybe as M (isJust)
-- http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Maybe.html#v:mapMaybe
import Data.List as L (intersect,nub)
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html
import Test.QuickCheck as Q
-- the module Test.QuickCheck provides us with tools for testing
-- whether a function we have designed is doing what it should.

deltaStar :: Monoid m => (q -> b -> Maybe (q,m)) -> (q,m) -> [b] -> Maybe (q,m)
deltaStar _ qm [] = return qm                 -- if there are no more steps
                                                                   -- to take, return the
                                                                   -- current state/output pair
deltaStar d (q,m) (b:bs) =                        -- otherwise,
  do
    (q',m') <- d q b                                       -- follow the next edge to a new state,
    deltaStar d (q',m `mappend` m') bs   -- update the output, and continue

data Fwd
data Bwd

data Direction d where
  Forward :: Direction Fwd
  Backward :: Direction Bwd

data SeqTrans q b m d where
  S :: Direction d -> [q] -> [b] -> m -> q -> m -> (q -> b -> Maybe (q,m)) -> (q -> Maybe m) -> SeqTrans q b m d

type ForwardTrans q b m = SeqTrans q b m Fwd
mkForwardTransducer ::  [q] -> [b] -> m -> q -> m -> (q -> b -> Maybe (q,m)) -> (q -> Maybe m) -> ForwardTrans q b m
mkForwardTransducer = S Forward
type BackwardTrans q b m = SeqTrans q b m Bwd
mkBackwardTransducer ::  [q] -> [b] -> m -> q -> m -> (q -> b -> Maybe (q,m)) -> (q -> Maybe m) -> BackwardTrans q b m
mkBackwardTransducer = S Backward

direction :: SeqTrans q b m d -> Direction d
direction (S dir _ _ _ _ _ _ _) = dir
states :: SeqTrans q b m d -> [q]
states (S _ qs _ _ _ _ _ _) = qs
inAlph :: SeqTrans q b m d -> [b]
inAlph (S _ _ iA _ _ _ _ _) = iA
outAlph :: SeqTrans q b m d -> m
outAlph (S _ _ _ oA _ _ _ _) = oA
start :: SeqTrans q b m d -> q
start (S _ _ _ _ st _ _ _) = st
initial :: SeqTrans q b m d -> m
initial (S _ _ _ _ _ i _ _) = i
delta :: SeqTrans q b m d -> (q -> b -> Maybe (q,m))
delta (S _ _ _ _ _ _ d _) = d
final :: SeqTrans q b m d -> (q -> Maybe m)
final (S _ _ _ _ _ _ _ f) = f

reverseDirection :: BackwardTrans q b m -> ForwardTrans q b m
reverseDirection (S Backward s iA oA st i d f) = mkForwardTransducer s iA oA st i d f

transduce :: Monoid m => SeqTrans q b m d -> [b] -> Maybe m
transduce t@(S Forward _ _ _ _ _ _ _) bs =
  do
    (q,m) <- deltaStar (delta t) (start t,initial t) bs 
    m' <- final t q                                      
    return (m `mappend` m')

compose :: Monoid m => SeqTrans q b [c] d  -> SeqTrans r c m d -> Maybe (SeqTrans (q,r) b m d)
compose s t =
  let
    newStates = [(q,r) | q <- states s, r <- states t]
    newDelta (q,r) b = 
      do
        (q',cs) <- delta s q b
        (r',m) <- deltaStar (delta t) (r,mempty) cs
        return ((q',r'),m)
    newFinal (q,r) = 
      do
        cs <- final s q 
        (r',m) <- deltaStar (delta t) (r,mempty) cs
        m' <- final t r'
        return (m `mappend` m')
  in 
    do
      initialOutput <- transduce t (initial s)
      return (S
              (direction s)
               newStates
               (inAlph s)
               (outAlph t)
               (start s, start t)
               initialOutput
               newDelta
               newFinal)

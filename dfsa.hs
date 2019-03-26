-- DFSA
-- Greg Kobele
-- 28 Nov 2018 

-- This file provides a module implementing deterministic finite state
-- automata, and is based on code by Jeff Heinz

module Dfsa
( DFSA(A)
, states
, sigma
, start
, finals
, delta
, recognizes
, intersection
, union
, complement
, difference
, subset
, isEmpty
) where

import Data.Foldable as F (foldlM)
-- http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Foldable.html#v:foldlM
import Data.Maybe as M (catMaybes)
-- http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Maybe.html#v:catMaybes
import Data.List as L (intersect,nub)
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html

import Prelude hiding (product)

data DFSA q b = A [q] [b] q [q] (q -> b -> Maybe q)

states :: DFSA q b -> [q]
states (A qs _ _ _ _) = qs 

sigma :: DFSA q b -> [b]
sigma (A _ bs _ _ _) = bs 

start :: DFSA q b -> q
start (A _ _ q0 _ _) = q0

finals :: DFSA q b -> [q]
finals (A _ _ _ fs _) = fs

delta :: DFSA q b -> q -> b -> Maybe q
delta (A _ _ _ _ d) = d

deltaStar :: (q -> b -> Maybe q) -> q -> [b] -> Maybe q
deltaStar = F.foldlM
-- deltaStar delta q [] = q
-- deltaStar delta q (b:bs) = case delta q b of
--                             Nothing -> Nothing
--                             Just q' -> deltaStar delta q' bs

recognizes :: (Eq q, Eq s) => DFSA q s -> [s] -> Bool
a `recognizes` input
  | all (`elem` sigma a) input =
    case deltaStar (delta a) (start a) input of
      Nothing -> False
      Just q -> q `elem` finals a
  | otherwise = False

complete :: DFSA q b -> DFSA (Maybe q) b
complete a = A qs bs (Just $ start a) fs d
  where
    qs = Nothing : fmap Just (states a)
    bs = sigma a
    fs = fmap Just (finals a)
    d Nothing _ = Just Nothing
    d (Just q) b = Just $ delta a q b

product ::(Eq q,Eq r) => (Bool -> Bool -> Bool) -> DFSA q b -> DFSA r b -> DFSA (q,r) b
product isFinal a1 a2 = A qs bs q0 fs d
  where
    qs = [(q1,q2) | q1 <- states a1, q2 <- states a2]
    bs = sigma a1 -- should be the same as sigma a2!!!
    q0 = (start a1,start a2)
    fs = filter (\(q1,q2) -> isFinal
                  (q1 `elem` finals a1)
                  (q2 `elem` finals a2)) qs
    d (x,y) b = 
      do
        q1 <- delta a1 x b
        q2 <- delta a2 y b
        return (q1,q2)

intersection :: (Eq q, Eq r) => DFSA q b -> DFSA r b -> DFSA (q,r) b
intersection = product (&&)

union :: (Eq q, Eq r) => DFSA q b -> DFSA r b -> DFSA (q,r) b
union = product (||)

difference :: (Eq q, Eq r) => DFSA q b -> DFSA r b -> DFSA (q,r) b
difference = product (\b1 b2 -> b1 && not b2)

complement :: Eq q => DFSA q b -> DFSA (Maybe q,Maybe q) b
complement a = product (const . not) ca ca
  where
    ca = complete a

closure :: Eq a => (a -> a) -> a -> a
closure f a = fst $ head $ filter (uncurry (==)) $ zip steps (tail steps)
  where
    steps = iterate f a

reachable :: Eq q => DFSA q b -> [q]
reachable a = closure (L.nub . concat . fmap getSuccessors) [start a]
  where
    getSuccessors q = M.catMaybes $ fmap (delta a q) $ sigma a

isEmpty :: Eq q => DFSA q b -> Bool
isEmpty a = null (finals a `L.intersect` reachable a)

subset :: (Eq q,Eq r) => DFSA q b -> DFSA r b -> Bool
subset a1 a2 = isEmpty $ difference a1 a2

equivalent :: (Eq q,Eq r) => DFSA q b -> DFSA r b -> Bool
equivalent a1 a2 = subset a1 a2 && subset a2 a1

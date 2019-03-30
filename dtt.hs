-- DTT
-- Greg Kobele
-- 04 Feb 2019

-- This file provides a module implementing (bottom up) tree  transducers.

{-# LANGUAGE PatternSynonyms #-}

module DTT (
  mkAcceptor
  , BUTA -- hinzugefügt
  , mkBUTA -- hinzugefügt
  , acceptBUTA -- hinzugefügt
  , mkBUTrans
  , mkMultTrans
  , mkMacroTrans
  , Tree(Node)
  , Context(NodeC,Hole)
  , CContext(NodeCC,CHole0,CHole1)
)

where

import Control.Monad as M (guard)
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad.html#v:guard
import Data.List as L (nub)
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html#v:nub

get :: [a] -> Int -> Maybe a
get xs i
  | i < 0 = Nothing
  | otherwise = get_ xs i
  where
    get_ [] i = Nothing
    get_ (x:_) 0 = Just x
    get_ (_:xs) n = get_ xs (n - 1)

data Tree a = Node a [Tree a] deriving (Show,Eq)

type Kernel a b = a -> [b] -> Maybe b
type Hom a b = Tree a -> Maybe b
extend :: Kernel a b -> Hom a b
extend ker (Node a ts) = traverse (extend ker) ts >>= ker a
{- traverse :: (a -> Maybe b) -> [a] -> Maybe [b]
if any element of the input list is mapped to @Nothing@, then the output is @Nothing@.
otherwise, behaves as @map@, ignoring the @Just@ wrapped around the outputs:
traverse f [] = Just []
traverse f (b:bs) =
  case f b of
    Nothing -> Nothing
    Just x ->
      case traverse f bs of
        Nothing -> Nothing
        Just xs -> Just (x : xs)

(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
If the input is @Nothing@, so is the output.
Otherwise, applies the function inside of the @Just@:
Nothing >>= f = Nothing
Just x >>= f = f x
-}

type AccRule a b = ((a,[b]),b)
mkAccKernel :: (Eq a, Eq b) => [AccRule a b] -> Kernel a b
mkAccKernel rs a bs = lookup (a,bs) rs
{- lookup :: a -> [(a,b)] -> Maybe b

The function @lookup@ takes an object and a list of pairs, and
searches for a pair whose first component is the object we were given.
In case one is found, the second component is returned (wrapped in a
@Just@).  If no such pair is present, @Nothing@ is returned.
-}

mkAcceptingHom :: (Eq a, Eq states) => [states] -> [AccRule a states] -> Hom a Bool
mkAcceptingHom finals rs t = (`elem` finals) <$> extend (mkAccKernel rs) t
{- (<$>) :: (a -> b) -> Maybe a -> Maybe b

This is another name for the function @fmap@, which applies a function
to a @Maybe@ argument:
fmap f Nothing = Nothing
fmap f (Just x) = Just (f x)
-}

acceptOrReject :: Hom a Bool -> Tree a -> Bool
acceptOrReject h = or . h

mkAcceptor :: (Eq a, Eq states) => [AccRule a states] -> [states] -> Tree a -> Bool
mkAcceptor rs fins = acceptOrReject $ mkAcceptingHom fins rs

data BUTA states alph =
  BUTA {stateSet :: [states]
       , finals :: [states]
       , alphabet :: [alph]
       , transitions :: alph -> [states] -> Maybe states}

deltaBUTA :: (a -> [states] -> Maybe states) -> Tree a -> Maybe states
deltaBUTA d (Node a ts) = 
  do
    states <- traverse (deltaBUTA d) ts
    -- first, run the machine on the daughters
    s <- d a states
    -- then, determine the state of the tree based on the states of its daughters
    return s

acceptBUTA :: Eq states => BUTA states a -> Tree a -> Bool
acceptBUTA bu = any (`elem` finals bu) . deltaBUTA (transitions bu)
  {- 
    @any@ applies a property to each element of a structure, and
    returns @True@ just in case the property was true of some element.
    In our case, we have an object of type @Maybe states@, which may
    or may not contain an object of type @states@.  So @any@ will be
    true just in case the object is of the form @Just s@, and @s@ is
    an element of the final state set.
-}

mkBUTA :: (Eq states, Eq alph) => [AccRule alph states] -> [states] -> BUTA states alph
mkBUTA rs fins = BUTA allStates fins letters (mkAccKernel rs)
  where
    (allLettersDups,allStatesDups) = unzip $ fmap (\((a,bs),c) -> (a,c:bs)) rs
    letters = L.nub allLettersDups
    allStates = L.nub $ concat (fins : allStatesDups)

data Context a = 
  NodeC a [Context a] | Hole Int 
  deriving (Show,Eq)

tree_substitution :: Context a -> [Tree a] -> Maybe (Tree a)
-- when you have a hole with name i, put the ith element from the list in the hole, if one exists
tree_substitution (Hole i) g =  get g i
-- if you have a tree, keep the node label, and substitute the holes in its daughters
tree_substitution (NodeC a cs) g = 
  do
    dtrs <- flip tree_substitution g `traverse` cs
    return (Node a dtrs)

getNodes :: Context a -> [a]
getNodes (Hole _) = []
getNodes (NodeC a cs) = a : concat (getNodes <$> cs)

type BURule inputNode state outputNode = ((inputNode,[state]),(state,Context outputNode))

mkBUKernel :: (Eq i,Eq s) => [BURule i s o] -> Kernel i (s,Tree o)
mkBUKernel rules a bs = 
  do
    (s,c) <- lookup (a,states) rules
    t <- tree_substitution c trees
    return (s,t)
  where
    (states,trees) = unzip bs

transduce :: Eq s => [s] -> Hom a (s,b) -> Hom a b
transduce fs h t =
  do
    (s,t') <- h t
    guard (s `elem` fs)
    return t'

{- guard :: Bool -> Maybe ()

@guard@ takes an expression and evaluates its truth.  If it is @True@,
computation proceedes.  If it is @False@, then @Nothing@ is returned.
-}

mkBUTrans :: (Eq a,Eq s) => [BURule a s b] -> [s] -> Hom a (Tree b)
mkBUTrans rs fins = transduce fins $ extend $ mkBUKernel rs

executeHom :: Hom a b -> (b -> Bool) -> (b -> c) -> Hom a c
executeHom h accept modify t = 
  do
    t' <- h t
    M.guard (accept t')
    return $ modify t'
{- 
Then
@transduce fs h = executeHom h ((`elem` fs) . fst) snd@
and
@mkAcceptingHom fs rs = executeHom (extend $ mkAccKernel rs) (`elem` finals) id@
-}

data BU s i o =
  BU {states :: [s]
     , iAlph :: [i]
     , oAlph :: [o]
     , finalSts :: [s]
     , trans :: i -> [s] -> Maybe (s,Context o)}

mkBU :: (Eq i,Eq s,Eq o) => [BURule i s o] -> [s] -> BU s i o
mkBU rs fins = BU sts iAl oAl fins tr
  where
    (stsDups,iAlDups,oAlDups) = unzip3 $ map (\((i,ds),(s,c)) -> (s:ds,i, getNodes c)) rs
    sts = L.nub $ concat $ stsDups
    iAl = L.nub iAlDups
    oAl = L.nub $ concat oAlDups
    tr = curry (flip lookup rs)

deltaBU :: (i -> [s] -> Maybe (s,Context o)) -> Hom i (s,Tree o)
deltaBU d (Node a ts) =
  do
    (states,trees) <- unzip <$> traverse (deltaBU d) ts
    (s,ctxt) <- d a states
    t <- tree_substitution ctxt trees
    return (s,t)

type HomRule a b = ((a,Int),Context b)

mkHomKernel :: Eq a => [HomRule a b] -> Kernel a (Tree b)
mkHomKernel rules a bs =
  do
    t <- lookup (a,len) rules
    t' <- tree_substitution t bs 
    return t'
  where
    len = length bs

type MultRule i s o = ((i,[(s,Int)]),(s,[Context o]))

mkMultKernel :: (Eq i,Eq s,Eq o) => [MultRule i s o] -> Kernel i (s,[Tree o])
mkMultKernel rules i bs = 
  do
    (sn,cs) <- lookup (i,ss) rules
    ts <- flip tree_substitution (concat trees) `traverse` cs
    return (sn,ts)
  where
    (states,trees) = unzip bs
    ss = zip states $ fmap length trees

mkMultTrans :: (Eq a,Eq s,Eq b) => [MultRule a s b] -> [s] -> Hom a ([Tree b])
mkMultTrans rs fins = transduce fins $ extend $ mkMultKernel rs

data MultBU s i o =
  MultBU {statesMult :: [s]
         , iAlphMult :: [i]
         , oAlphMult :: [o]
         , finalStsMult :: [s]
         , transMult :: i -> [(s,Int)] -> Maybe (s,[Context o])}

mkMultBU :: (Eq i,Eq s,Eq o) => [MultRule i s o] -> [s] -> MultBU s i o
mkMultBU rs fins = MultBU sts iAl oAl fins tr
  where
    (iAlDups,stsDups,oAlDups) = unzip3 $ fmap (\((i,ss),(s,ctxs)) -> (i,s:(fmap fst ss),ctxs >>= getNodes)) rs
    sts = L.nub $ concat $ stsDups
    iAl = L.nub iAlDups
    oAl = L.nub $ concat oAlDups
    tr = curry (flip lookup rs)

data CContext a = 
  NodeCC a [CContext a] | CHole0 Int | CHole1 Int [CContext a]
  deriving (Show,Eq)

ctxt_composition :: Context a -> [Context a] -> Maybe (Context a)
ctxt_composition (Hole i) g = get g i
ctxt_composition (NodeC a cs) g = 
  do
    dtrs <- flip ctxt_composition g `traverse` cs
    return (NodeC a dtrs)

cCtxt_substitution :: CContext a -> [Context a] -> Maybe (Context a)
-- if you are a leaf hole, that stays the same
cCtxt_substitution (CHole0 i) _ =  Just (Hole i)

-- if you are a node, just do the substitution on all daughters
cCtxt_substitution (NodeCC a ccs) cs = 
  do
    dtrs <- flip cCtxt_substitution cs `traverse` ccs
    -- doing the substitution on all daughters
    return (NodeC a dtrs)

-- if you are an internal hole, replace it with the appropriate
-- context (obtained via @get cs i@).  Because the daughters of this
-- guy are still @CContext@s, we need to turn them into @Contexts@ by
-- substituting into the open @CHole1@s the appropriate contexts from
-- @cs@.  Then we can compose the contexts.
cCtxt_substitution (CHole1 i ccs) cs =
  do
    ctx <- get cs i
    -- getting the appropriate context to replace the internal hole
    ctxts <- flip cCtxt_substitution cs `traverse` ccs
    -- turning the daughters of the internal hole into normal contexts
    ctxt_composition ctx ctxts

type MacroRule i s o = ((i,[s]),(s,CContext o))

mkMacroKernel :: (Eq i,Eq s) => [MacroRule i s o] -> Kernel i (s,Context o)
mkMacroKernel rules a bs = 
  do
    (s,cctx) <- lookup (a,states) rules
    ctx <- cCtxt_substitution cctx ctxts
    return (s,ctx)
  where
    (states,ctxts) = unzip bs

mkMacroTrans :: (Eq i,Eq s) => [MacroRule i s o] -> [s] -> Hom i (Context o)
mkMacroTrans rs fins = transduce fins $ extend $ mkMacroKernel rs

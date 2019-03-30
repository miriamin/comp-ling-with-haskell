import DTT

-- Suffix ordering in English morphology: A bottom up tree acceptor

{-
This project is a bottom up tree acceptor to describe the contraints in level ordering for english suffixes.


The constraints for suffix-suffix combination in english are a morphological phenomenon that until today is 
discussed controversially by linguistic scholars. One group argues for a level-ordering of English morphology 
(Hay & Plag 2004, Fabb 1988). Within this theoretic frame, English suffixes are divided into two distict sets,
that are often called Level 1 Affixes and Level 2 Affixes. 

The generalization about the order in which these suffixes appear is that .. 



This project implements the suffix ordering rules suggested by the level-ordering theory. 


-- Description of the generalization 


-}
-- states: 'a' -> has no suffixes , 'b' --> has level 1 suffix , 'c' --> has level 2 suffix

data Suffix_States = Stem | L1 | L2| RightOrder | WrongOrder deriving (Eq)

level1Suffixes :: [String]
level1Suffixes = ["al", "an", "ant", "ion"]

level2Suffixes :: [String]
level2Suffixes = ["hood", "ist" , "ize"]

stems :: [String]
stems = ["revolut", "devot"]

acceptorRules =
  [(('a',[]),Stem) -- a word with no suffixes is accepted
  , (('b',[]),L1) -- only level1suffix is not accepted
  , (('c',[]),L2) -- only level2suffix is not accepted
  , (('f',[Stem,L1]),L1) 
  , (('f',[Stem,L2]),L2) 
  , (('f',[L2,Stem]),WrongOrder) 
  , (('f',[L1,Stem]),WrongOrder)
  , (('f',[Stem,Stem]),WrongOrder) 
  , (('f',[L1,L2]),RightOrder) 
  , (('f',[L2,L1]),WrongOrder) 
  ]



acceptor = mkAcceptor acceptorRules [Stem, RightOrder] -- nimmt rules und accepted states


test :: BUTA Suffix_States Char
test = mkBUTA acceptorRules [Stem, RightOrder] 

-- "revolution"
-- acceptBUTA test (Node 'f' [Node 'a' [], Node 'b' []]) 
-- acceptBUTA test (Node 'a' [])
-- acceptBUTA test (Node 'f' [Node 'f' [Node 'a' [], Node 'b' []], Node 'c' []]) -- revolutionary

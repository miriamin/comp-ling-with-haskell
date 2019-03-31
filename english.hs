
-- Suffix ordering in English morphology: A bottom up tree acceptor

{-
This project is a bottom up tree acceptor to describe the contraints in level ordering for english suffixes.


Description of the phenonemon and generalization
-------
The constraints for suffix-suffix combination in english are a morphological phenomenon that until today is 
discussed controversially by linguistic scholars. The general observation is that there are different groups of suffixes
and that suffixes of group 1 cannot attach to a word to which a suffix of group 2 has already been attached.

Examples:
revolut - ion - ist
* devot - ist - ion

Some scholars argue that these combinatorial restrictions can be explained by the fact that English suffixes 
are organized in different lexical strata, also known as the level ordering theory (Hay & Plag 2004, Griegerich 1999). 
These strata interact phonolohgically and morphologically and thus lead to combinatorial constraints.
According to the level-ordering hypothesis, English suffixes belong to the following classes:

Class I suffixes: -ion, -ity, -y, -al, -ic, -ate, -ous, -ive, -able, -ize
Class II suffixes: -ness, -less, -hood, -ful, -ly, -y, -like, -ist, -able, -ize, -ary
(Spencer 1991)


The generalization about the order in which these suffixes appear is that class 1 (or level 1) suffixes are first 
attached to the stem and then class 2(or level 2) affixes are attaches. That means, that level 1 affixes cannot 
attach to a word  to which a level 2 affix has already been attached. Level 1 - leve 1 and level 2 - level 2 combinations
are valid. 

 
This project implements the suffix ordering rules suggested by the level-ordering theory. 



Implementation
-----
To implement this morphological phenonemon in Haskell I used the Definite Tree Transducer (DTT) module by
Greg Kobele

First, I define the two suffix sets and the stems where they can be attached as functions.
The list can easyly be expanded by further morphems.

-}
import DTT


level1Suffixes :: [String]
level1Suffixes = ["ion", "ity", "y", "al", "ic", "ate", "ous", "ive", "able", "ize"]

level2Suffixes :: [String]
level2Suffixes = [ "ness", "less", "ness" ,"hood", "ful", "ly", "y", "like", "ist", "able", "ize", "ary"]

stems :: [String]
stems = ["revolut", "devot", "lone", "pract"]

{- 
The ordering rules are modeled in the function acceptorRules. It takes a symbol and an array of stated and evaluates
them as a state.

I defined the following states that help the acceptor first parse the input as Stem, level 1 or level 2 suffix and
then evaluate the combination of the states correctly.
-}

data Suffix_States = Stem | L1 | L2| RightOrder | WrongOrder deriving (Eq)

{-
The symbos 'a', 'b', and 'c' are the leaf nodes that contain the input morphems (stem, level 1 suffix, level 2 suffix
in that order). 'f's are the node leafs. 

-}
acceptorRules =
  [(('a',[]),Stem)
  , (('b',[]),L1)
  , (('c',[]),L2)
  , (('f',[Stem,L1]),L1) 
  , (('f',[Stem,L2]),L2) 
  , (('f',[L2,Stem]),WrongOrder) 
  , (('f',[L1,Stem]),WrongOrder)
  , (('f',[Stem,Stem]),WrongOrder) 
  , (('f',[L1,L2]),RightOrder) 
  , (('f',[L2,L1]),WrongOrder) 
  , (('f',[L2,L2]),RightOrder) 
  , (('f',[L1,L1]),RightOrder) 
  ]


{-
Testing 
------

The acceptor can be called on the command line with the function checkStrata. It takes three Strings (one stem and 
two suffixes) and evaluates True, if they represent the correct suffix oder and False if the order is not correct. 

Examples:
L1<L2
checkStrata "revolut" "ion" "ary" --> True

L2<L1
checkStrata "devote" "ist" "ion" --> False

L1<L1
checkStrata "pract" "ic" "al" --> True

L2<L2
checkStrata "lone" "ly" "ness"


-}

test :: BUTA Suffix_States Char
test = mkBUTA acceptorRules [Stem, RightOrder] 


checkStrata :: String -> String -> String -> Bool
checkStrata a b c 
  | elem a stems && elem b level1Suffixes && elem c level2Suffixes = acceptBUTA test (Node 'f' [Node 'f' [Node 'a' [], Node 'b' []], Node 'c' []])
  | elem a stems && elem b level2Suffixes && elem c level1Suffixes = acceptBUTA test (Node 'f' [Node 'f' [Node 'a' [], Node 'c' []], Node 'b' []])
  | elem a stems && elem b level2Suffixes && elem c level2Suffixes = acceptBUTA test (Node 'f' [Node 'f' [Node 'a' [], Node 'c' []], Node 'c' []])
  | elem a stems && elem b level1Suffixes && elem c level1Suffixes = acceptBUTA test (Node 'f' [Node 'f' [Node 'a' [], Node 'b' []], Node 'b' []])
  | otherwise = error "invalid input"


{-
References

Jennifer Hay and Ingo Plag, 2004: What Constrains Possible Suffix Combinations? On the Interaction of Grammatical and
Processing Restrictions in Derivational Morphology. In: Natural Language & Linguistic Theory, Vol. 22, No. 3 (Aug., 2004), pp. 565-596

Giegerich, Heinz J. 1999. Lexical Strata in English. Morphological Causes, PhonologicalEffects, Cambridge University Press, Cambridge.

Spencer, Andrew. 1991. Morphological Theory: An introduction to Word Structure in Generative Grammar. Oxford: Blackwell.

-}

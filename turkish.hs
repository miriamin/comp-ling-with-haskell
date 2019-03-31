{-
A string acceptor for Turkish phonological rules
------------------------------------------------



This project is a deterministic finite state acceptor for selected phonological rules in standard Turkish.
The machine is able to accept the well-formedness of strings regarding final devoicing, vowel roundness harmnony
and the elemination of initial consonant clusters.

First I define the phonems the automata need. The acceptor is working for orthographic as well as phonetic script.

-}

voiced :: [Char]
voiced = "gdbcğγd͡ʒ"

alphabet :: [Char]
alphabet = ['a'..'z'] ++ unrounded ++ rounded ++ voiced ++ "-"

unrounded :: [Char]
unrounded = "eiaıɯ"

rounded :: [Char]
rounded = "øöüyuo"

vowels :: [Char]
vowels = rounded ++ unrounded


{-
Here is the DFA interface to implement the three automata.
-}

type DFA state symb = ([state], [symb], state, [state], (state, symb) -> state)

recognizes :: Eq q => DFA q s -> [s] -> Bool
a@(qs, sigma, q0, fs, delta) `recognizes` input =
  deltaStar (q0, input) `elem` fs
  where
      deltaStar = uncurry $ foldl (curry delta)

{-
1. Final devoicing: generalization and implementation
--------

As found for example in Göksel and Kerslake 2005 p. 14f or Lewis 2001 p. 10f, voiced consonants (/d/, /g/, /b/,/γ/, /d͡ʒ/)
never occur in final position. They undergo a devoicing process to become their unvoiced counterparts /t/, /k/, /p/, /x/ and /t͡ʃ/.

For this automata I defined 3 states: 
0: Start (no phoneme seen), 
1: Seen consonant that is not +voiced 
2: Seen +voiced consonant. 

Only state 1 can be a final state. 
-}

finalDevoicing :: DFA Int Char
finalDevoicing = ([0,1,2], alphabet, 0, [1], d)
 where
  d (0, x) | x `elem` voiced = 2
           | otherwise = 1
  d (1, x) | x `elem` voiced = 2
           | otherwise = 1
  d (2, x) | x `elem` voiced = 2
           | otherwise = 1

{-
2. Roundedness Harmony: generalization and implementation
--------

Turkish knows two types of vowel harmony: fronting/backness harmony and roundedness harmony as for exmple described in 
Göksel and Kerslake 2005 p. 21.
In a range of languages vowels harmonize in different distinct features. Scholars argue that this assimilation process 
is due to the economy of musculat effort (Lewis 2001 p.15f). 
My automata recognizes vowel roundness harmony in Turkish. Roundedness harmony is a phonological process that determines the 
roundedness of all vowels in one word depending of the roundedness of the first vowel. As a consequence, in every Turkish word,
all vowels agree in the feature of roundedness.  

Unrounded vowels in Turkish: /e/, /i/, /a/, /ɯ/
Rounded vowels in Turksih: /ø/, /y/, /u/, /o/

To model roundedness harmony I defined 5 states:
0: Start (nothing seen)
1: no vowel seen
2: -round vowel seen
3: +round vowel seen
99: dead state

States 1, 2 and 3 can be final states. 
-}

roundnessHarmony :: DFA Int Char
roundnessHarmony = ([0,1,2,3,99],alphabet,0,[1,2,3],d)
 where
  d (0, x) | elem x unrounded = 2
           | elem x rounded = 3
           | otherwise = 1
  d (1, x) | elem x unrounded = 2
           | elem x rounded = 3
           | otherwise = 1    
  d (2, x) | elem x unrounded = 2
           | elem x rounded = 99
           | otherwise = 2
  d (3, x) | elem x unrounded = 99
           | elem x rounded = 3
           | otherwise = 3  
  d (99, x) = 99   
  
  
{-
Initial cluster contraint: generalization and implementation
-------

As stated in Lewis 2001: "Two consonants never occur together at the beginning of a native word." (Lewis 2001 p9)
In loan words, speakers repair initial cononant clusters with vowel epenthesis, eg /stres/ -> /sitres/ (Göksel and Kerslake 2005 p 12).

The automata uses 4 states:
0: Start
1: Inital vowel and all phonems after inital position
2: Initial consonant
99: Dead (consonant in position 2)

Only state 1 is a final state, assuming that a single consonant is not a valid word. 
-}
  
rmInitialCons :: DFA Int Char     
rmInitialCons = ([0,1,2,99],alphabet,0,[1],d)
 where
  d (0, x) | elem x vowels = 1
           | otherwise = 2
  d (1, x) = 1
  d (2, x) | elem x vowels = 1
           | otherwise = 99
  d (99, x) = 99     


           
{-
Verification
---------
 
Here is a function that takes one string and has it verfied by all three acceptors. It evaluated the input as True,
when all automata accept the input. 
-}

turkish :: [Char] -> Bool
turkish xs = finalDevoicing `Main.recognizes` xs 
          && roundnessHarmony `Main.recognizes` xs 
          && rmInitialCons `Main.recognizes` xs             

turkish2 :: [Char] -> Bool
turkish2 xs = roundnessHarmony `Main.recognizes` xs   

{-
References

Geoffrey Lewis, 2002: Turkish Grammar. 2nd edition, Oxford University Press.

Aslı Göksel and Celia Kerslake, 2005: Turkish: A comprehensive grammar. Routledge. 

-}

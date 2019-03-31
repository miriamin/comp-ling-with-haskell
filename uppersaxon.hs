{-
A string transducer for Upper Saxon phonology
--------------------


This project is a string transducer that takes a standard German word and returns it saxonized.
The machine implements three phonological rules for Upper Saxon described in Khan and Weise 2013.
Upper Saxon is a group of dialects spoken by 2 million people in and around Chemnitz, a city in Sachsen, 
Eastern Germany. 

The machine is based on three phonological characteristics in Upper Saxon that do not exist
in Standard German and implements the processes that transform Standard German into Upper Saxon:
1. Lacking of voicing contrast in stops
2. No contrast in initial clusters with /t k/ ans /l/
3. /ʃ/ cluster replacing final /iç/ and /iʃ/

I use the deterministic finite state transducer module by Greg Kobele as interface for the transducers in the project.
-}

import Dfst
import Data.Maybe

{-
Here are the relevant phoneme classes for later use.
-}
sonorant s = vowel s || (s == 'm') || (s == 'n') || (s == 'ŋ') || (s == 'l') || (s == 'r') || (s == 'j') || (s == 'w') 
notSonorant = not . sonorant
vowel s = (s == 'a') || (s == 'e') || (s == 'i') || (s == 'o') || (s == 'u')
voicedObstruent s = (s == 'b') || (s == 'd') || (s == 'g') 
germanFinal s = (s=='ç') || (s=='ʃ') || (s == 'k')


{-
1. No voiced plosives: generalization and implementation
----------

As described an Khan and Weise: "Unlike Standard German, Upper Saxon lacks voicing contrast." They argue that underlying /b d g/ "is most commonly
realized as /p t k/". They remain sometimes voiced between sonorants, but because there are no futher information given on that behalf, this exception
shall be ignored here.

I provide the transducer with a function that unvoices the plosives /b d g/ and define the states for the transducer:
UV_Zero: Start
UV_Phoneme: every phoneme after Start
-}
unvoice :: Char -> Char
unvoice obstruent
 |obstruent == 'd' = 't'
 |obstruent == 'b' = 'p'
 |obstruent == 'g' = 'k'
 |otherwise = obstruent
 
data UV_States = UV_Zero | UV_Phoneme

rmVoicing:: ForwardTrans UV_States Char String
rmVoicing = mkForwardTransducer [UV_Zero, UV_Phoneme] ['a'..'z'] ['a'..'z'] UV_Zero "" delta final
  where
    final _ = Just []
    delta UV_Zero c | voicedObstruent c = Just (UV_Phoneme, [unvoice c])
                 | otherwise = Just (UV_Phoneme, [c])
    delta UV_Phoneme c | voicedObstruent c = Just (UV_Phoneme, [unvoice c])
                 | otherwise = Just (UV_Phoneme, [c])


{-
Initial clusters with /l/: Generalization and implementation

Khan and Weise found that "in initial clusters with /l/, stops /t k/ do not contrast and can vary freely". As an example
they give /klʌ:s/ = /tlʌ:s/ ('glass') (Khan and Weise 2013 p. 234). My transducer transforms instances of initial /kl/
into /tl/. 

It uses the following states: 
TL_Zero: Start
TL_SeenInitialK: Initial /k/
TL_SeenNone: Every other phonem then an initial /k/
TL_SeenLafterInitialK : an /l/ seen after an inital /k/
-}

data TL_States = TL_Zero | TL_SeenInitialK | TL_SeenNone | TL_SeenLafterInitialK 

tlize:: ForwardTrans TL_States Char String
tlize = mkForwardTransducer [TL_Zero, TL_SeenInitialK, TL_SeenNone, TL_SeenLafterInitialK] ['a'..'z'] ['a'..'z'] TL_Zero "" delta final
  where
    final _ = Just []
    delta TL_Zero c | (==) 'k' c = Just (TL_SeenInitialK , "")
                    | otherwise = Just (TL_SeenNone ,[c])
    delta TL_SeenInitialK c | (==) 'l' c = Just( TL_SeenLafterInitialK, "tl")
                            | otherwise = Just (TL_SeenNone, "k" ++ [c])
    delta TL_SeenLafterInitialK c = Just (TL_SeenNone, [c])
    delta TL_SeenNone c = Just (TL_SeenNone, [c])       

{-
Final /ʃ/ - clusters: Generalization and implementation

In Upper Saxon, final /ʃ/ -clusters can be found, that do not exist in Standard German (Khan and Weise 2013 p. 234).
They are formed when a final /-ɪk/, /-ɪʃ/ or /-ɪç/ is found in Standard German and replaces these, e.g. /ɛsɪk/ or /ɛsɪç/ 
'vinegar' in Standard German becomes /ɛsʃ/

Since I could not make the BackwardTrans from the DFST module working, the string input for this transducer must be
reversed before transduction (and the output rereversed again). That means that this machine is a forward Transducer that
transforms strings starting with /kɪ/ etc. 

The states for this automata are:
C_Zero: Start
C_SeenNone: No relevant ending seen
C_SeenFinal: Seen a consonant that could be a final /k ʃ ç/
C_SeenI: Seen an /ɪ/ after the relevant final
-}

data C_States = C_Zero | C_SeenNone | C_SeenI | C_SeenFinal Char

cluster::  ForwardTrans C_States Char String        
cluster = mkForwardTransducer states ['a'..'z'] ['a'..'z'] C_Zero [] delta final
  where
   states = (C_Zero : C_SeenNone : C_SeenI :[C_SeenFinal c | c <- ['a'..'z'], germanFinal c])
   final (C_SeenFinal b) = Just [b]
   final _ = Just ""
   delta C_Zero c | germanFinal c = Just (C_SeenFinal c, "")
                  | otherwise = Just (C_SeenNone, [c])
   delta C_SeenI c = Just (C_SeenNone, "ʃ" ++ [c])
   delta C_SeenNone c = Just (C_SeenNone, [c])
   delta (C_SeenFinal b) c | (==) 'ɪ' c = Just(C_SeenI, "")
                           | otherwise = Just (C_SeenNone, [b,c])
     
{-
Verification
----------

Here is a function that takes the three transducers and applies them all to a given string. Since there are no information about
rule ordering in the paper, I ordered them in a way it makes sense:

ʃ-Clustering >> /kl/->/tl/ >> NoVoicing

examples: 
saxonize "lustɪg" -> "lustʃ"
saxonize "klavier" -> "tlavier"
saxonize "durchdacht" -> "turchtacht"
-} 

saxonize :: String -> String
saxonize x = fromJust $ (transduce rmVoicing (fromJust $ (transduce tlize (reverse $ fromJust $ (transduce cluster (reverse x))))))

{-
References
-------
Sameer ud Dowla Khan und Constanze Weise, 2013: Upper Saxon. In: Journal of the International Phonetic Association, Volume 43, Issue 2
-}

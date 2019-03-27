module UpperSaxon where

-- model language project: https://home.uni-leipzig.de/gkobele/courses/2018.WS/CompLing/lomongo.html

-- https://home.uni-leipzig.de/gkobele/courses/2018.WS/CompLing/index.html

import Dfst
-- import IPA
--import IPA_Test
--import Data.Maybe

-- data set 1: upper saxon lacks voicing contrast in obstruents
-- Gasse: ˈgʌsə --> ˈkʌsə
-- Damm: dʌm --> tʌm
-- bellt: bɛlt --> pɛlt

-- Fricatives /f s ʃ X/  and obstruents /p t k/ are only (sometimes) voiced between sonorants (p. 232 ff)
-- never voiced word- initially or finally

sonorant s = vowel s || (s == 'm') || (s == 'n') || (s == 'ŋ') || (s == 'l') || (s == 'r') || (s == 'j') || (s == 'w') 
notSonorant = not . sonorant
vowel s = (s == 'a') || (s == 'e') || (s == 'i') || (s == 'o') || (s == 'u')
voicedObstruent s = (s == 'b') || (s == 'd') || (s == 'g') 

unvoice :: Char -> Char
unvoice obstruent
 |obstruent == 'd' = 't'
 |obstruent == 'b' = 'p'
 |obstruent == 'g' = 'k'
 |otherwise = obstruent
 

saxonize :: String -> String
saxonize [] = []
saxonize (x:xs) = unvoice x:saxonize xs





-- 3 transducer: 
-- 1.: fricative und obstruenten werden stimmlos, außer wenn sie zwischen 2 sonoranten stehen 
-- 2 states: 1. seenSonorant 2. notSeenSonorant
 
{-
data SonorStates = Zero | SeenSon | AfterSon

rmVoicing:: ForwardTrans SonorStates Char String
rmVoicing = mkForwardTransducer states ['a'..'z'] ['a'..'z'] Zero "" delta finalStates
  where
    states = (Zero : SeenSon : [AfterSon c | c <- ['a'..'z'], sonorant c])
    finalStates (AfterSon c) = Just[c]
    finalStates _ = Just ""
    delta Zero c | sonorant c = Just (SeenSon, [c])
             | otherwise = Just (Zero, [c])
    delta SeenSon c | sonorant c = Just(SeenSon, [c])
                | voicedObstruent c = Just(AfterSon c , "")
                | otherwise = Just (Zero, [c])
    delta (AfterSon b) c | sonorant c = Just(SeenSon, [b,c])
                     | otherwise = Just (Zero,[unvoice b,c])
-}

-- alle Obstruenten werden stimmlos
data UV_States = UV_Zero | UV_Phoneme
data TL_States = TL_Zero | TL_SeenInitialK | TL_SeenNone | TL_SeenLafterInitialK

rmVoicing:: ForwardTrans UV_States Char String
rmVoicing = mkForwardTransducer [UV_Zero, UV_Phoneme] ['a'..'z'] ['a'..'z'] UV_Zero "" delta final
  where
    final _ = Just []
    delta UV_Zero c | voicedObstruent c = Just (UV_Phoneme, [unvoice c])
                 | otherwise = Just (UV_Phoneme, [c])
    delta UV_Phoneme c | voicedObstruent c = Just (UV_Phoneme, [unvoice c])
                 | otherwise = Just (UV_Phoneme, [c])
                 
 
-- In initial clusters with /l/, stops /t k/ do not contrast: /kl/ -> /tl/ 
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
    
-- * final -ig/-ich/-isch in standard german becomes a /ʃ/-cluster (p 234)
-- since I could not make the BackwardTrans from dfst module working (recognizes function is not implemented for BackwardTrans?) 
-- I used a little hack (just reversed the string before)
                  
                 
-- testing: (transduce rmVoicing) input1
input1 = "der hund bellt"
output1 = "pellt"

-- testing: (transduce tlize) input2
input2 = "klavierlk"

-- testing: 
input3 = reverse "lustig"

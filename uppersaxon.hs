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

unvoice :: Char -> Char
unvoice obstruent
 |obstruent == 'd' = 't'
 |obstruent == 'b' = 'p'
 |obstruent == 'g' = 'k'
 |otherwise = obstruent

saxonize :: String -> String
saxonize [] = []
saxonize (x:xs) = rmVoicing x:saxonize xs

sonorant s = vowel s || (s == 'm') || (s == 'n') || (s == 'ŋ') || (s == 'l') || (s == 'r') || (s == 'j') || (s == 'w') 
notSonorant = not . sonorant
vowel s = (s == 'a') || (s == 'e') || (s == 'i') || (s == 'o') || (s == 'u')
voicedObstruent s = (s == 'b') || (s == 'd') || (s == 'g') 

data SonorStates = Zero | SeenSon | AfterSon

-- 3 transducer: 
-- 1.: fricative und obstruenten werden stimmlos, außer wenn sie zwischen 2 sonoranten stehen 
-- 2 states: 1. seenSonorant 2. notSeenSonorant
 
rmVoicing:: ForwardTrans SonorStates Char String
rmVoicing = mkForwardTransducer [Zero, SeenSon, AfterSon] ['a' .. 'z'] ['a' .. 'z'] Zero "" delta finalStates
  where
    finalStates _ = Just ""
    delta Zero c | sonorant c = Just (SeenSon, [c])
             | otherwise = Just (Zero, [c])
    delta SeenSon c | sonorant c = Just(SeenSon, [c])
                | voicedObstruent c = Just(AfterSon c , "")
                | otherwise = Just (Zero, [c])
    d (AfterSon b) c | sonorant c = Just(SeenSon, [b,c])
                     | otherwise = Just (Zero,[unvoice b,c])

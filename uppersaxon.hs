module UpperSaxon where

-- model language project: https://home.uni-leipzig.de/gkobele/courses/2018.WS/CompLing/lomongo.html

-- https://home.uni-leipzig.de/gkobele/courses/2018.WS/CompLing/index.html

-- import Dfst
-- import IPA
--import IPA_Test
--import Data.Maybe

-- data set 1: upper saxon lacks voicing contrast in obstruents
-- Gasse: ˈgʌsə --> ˈkʌsə
-- Damm: dʌm --> tʌm
-- bellt: bɛlt --> pɛlt

-- Fricatives /f s ʃ X/  and obstruents /p t k/ are only (sometimes) voiced between sonorants (p. 232 ff)
-- never voiced word- initially or finally

rmVoicing :: Char -> Char
rmVoicing obstruent
 |obstruent == 'd' = 't'
 |obstruent == 'b' = 'p'
 |obstruent == 'g' = 'k'
 |otherwise = obstruent

saxonize :: String -> String
saxonize [] = []
saxonize (x:xs) = rmVoicing x:saxonize xs

sonorant s = (s == 'm', /n/, /ŋ/ /l/  /r/  /j/ und /w/ 

-- 3 transducer: 
-- 1.: fricative und obstruenten werden stimmlos, außer wenn sie zwischen 2 sonoranten stehen 
-- 2 states: 1. seenSonorant 2. notSeenSonorant

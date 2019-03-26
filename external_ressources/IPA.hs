{-
International Phonetic Alphabet
Robert Max Polter
09 Dec 2018 - work in progress

Issues:
- Tone has heuristic implementation
IPA (Extra High Tone) = ThisFile (Falling Tone)
IPA (Extra Low Tone) = ThisFIle (Rising Tone)
- no documentation in module header
- diacritics, suprasegmentals and tone symbols are parsed as stand-alone symbols of type Modif, delta-functions in DFAs need to check for potential diacritics at every step of the way or leave them out entirely ):

To Do:
1. non-heuristic tone implementation
2. parseIPA behavior for non-IPA Chars
3. edit parseIPA and parseIPA2 to merge vowels and consonants with their respective diacritics, change data declaration of Cons and Vowel to sth like C Place Manner Voicing [Modif] - or maybe only possible for IPAc Cons & IPAv Vowel? probably -> research! implementation via DFA?
4. deal with non-superscript diacritics/symbols
5. find better solution for xSym and xSpec Lists; make expandable w/o disturbing matching - safe as tuples & xSym?
6. Change name of data Symbol and convert to xSym xSpec pattern - adjust parseIPA & parseIPA2 once completed
7. split this file up - maybe Consonants.hs, Vowels.hs, Modifiers.hs, "Symbols".hs (impending name change), IPA.hs? What to do with xtract-functions?
-. specify show for data IPA
-. implement different levels of boundaries
-. non-pulmonic consonants
-. more suprasegmentals
-. affricates
-. "other symbols"
-. parseIPA2 for Modif
X. write documentation in module header

-}
module IPA
  
-- write documentation once completed!
  

where

-- I use this package to implement IPA symbol-meaning mapping dictionaries. A qualified import means that whenever I call a function from this package I have to write it like so : Map.function
-- theoretically It would have been possible to use the lazy (reffering to lazy computing) implementation, but since the lists are relatively small and the values are comparably small/ do not require lazy computation the strict implementation provides better performance
-- lazy: expressions are only evaluated if unavoidable and never more than necessary
-- strict: expressions are evaluated (costs more memory space)
import qualified Data.Map.Strict as Map

-- a metatype for IPA symbols
data IPA = IPAc Cons | IPAv Vowel | IPAs Symbol | IPAm Modif
  deriving (Eq, Show, Read)

-- some features of consonants
data Place = Bilabial | Labiodental | Dental | Alveolar | Postalveolar | Retroflex | Palatal | Velar | Uvular | Pharyngeal | Glottal
  deriving (Eq, Show, Read)

data Manner = Plosive | Nasal | Trill | Tap | Fricative | Lateralfricative | Approximant | Lateralapproximant
  deriving (Eq, Show, Read)

data Voicing = Voiced | Voiceless
  deriving (Eq, Show, Read)

-- some features of vowels, deriving Ord for markedness
data Open = Close | CloseCloseMid | CloseMid | Mid | OpenMid | OpenOpenMid | Open
  deriving (Eq, Show, Read, Ord)

data Front = Front | FrontCentral | Central | BackCentral | Back
  deriving (Eq, Show, Read, Ord)

data Rounded = Rounded | Unrounded
  deriving (Eq, Show, Read)

-- a type declaration and some accessing functions for consonants
data Cons = C Place Manner Voicing | UnreadableCons
  deriving (Eq, Show, Read)

manner :: IPA -> Manner
manner (IPAc (C _ manner _)) = manner

place :: IPA -> Place
place (IPAc (C place _ _)) = place

voicing :: IPA -> Voicing
voicing (IPAc (C _ _ voicing)) = voicing

-- and another one for vowels including the according functions
data Vowel = V Open Front Rounded | UnreadableVowel
  deriving (Eq, Show, Read)

open :: IPA -> Open
open (IPAv (V open _ _)) = open

front :: IPA -> Front
front (IPAv (V _ front _)) = front

rounded :: IPA -> Rounded
rounded (IPAv (V _ _ rounded)) = rounded

-- diacritics, so far only diacritics for superscript symbols
-- basic suprasegmentals and tone TONE IS IMPLEMENTED OUTSIDE IPA STANDARD! extra high -> falling, extra low -> rising

-- a metatype for everything that might latch itself onto a consonant or a vowel
data Modif = Mdia Diac | Msup Supraseg | Mton Tone
  deriving (Eq, Show, Read)

data Diac = Aspirated | Labialized | Palatalized | Velarized | Pharyngealized | NasalRelease | LateralRelease | UnreadableDiacritic
  deriving (Eq, Show, Read)

data Supraseg = Long | HalfLong | UnreadableSuprasegmental
  deriving (Eq, Show, Read, Ord)

data Tone = FallingTone | HighTone | MidTone | LowTone | RisingTone | UnreadableTone
  deriving (Eq, Show, Read, Ord)

-- I needed these functions because I was stupid and did all the typing before thinking about a good data structure so whatevs, we might need some of these for more complex stuff

tuplify :: [a] -> [b] -> [(a, b)]
tuplify (x:[]) (y:[]) = [(x,y)]
tuplify (x:xs) (y:ys) = [(x,y)] ++ tuplify xs ys

xtractcuzimlazy :: [(Char, b)] -> IO ()
xtractcuzimlazy ((a,b):[]) = putStrLn $ "'"++[a]++"'"
xtractcuzimlazy ((a,b):xs) =
  do
    putStr $ "'"++[a]++"',"
    xtractcuzimlazy xs

xtractimtheworst :: [(a1, a2)] -> [a2]
xtractimtheworst ((a,b):[]) = [b]
xtractimtheworst ((a,b):xs) = [b] ++ xtractimtheworst xs

xtract3 :: [Char] -> IO ()
xtract3 (x:[]) = putStrLn $ "'"++[x]++"'"
xtract3 (x:xs) =
  do
    putStr $ "'"++[x]++"',"
    xtract3 xs

comp :: Eq a => [a] -> [a] -> [Bool]
comp (x:[]) (y:[]) | x == y = [True]
                   | otherwise = [False]
comp (x:xs) (y:ys) | x == y = [True] ++ comp xs ys
                   | otherwise = [False] ++ comp xs ys

xtract4 :: Show a => [a] -> IO ()
xtract4 [] = putStr ""
xtract4 (x:xs) = do
  putStrLn $ show x
  xtract4 xs

xtract5 :: Show a => [a] -> IO ()
xtract5 [] = putStr ""
xtract5 (x:xs) = do
  putStrLn $ checking $ show x
  xtract5 xs

checking :: [Char] -> [Char]
checking [] = []
checking (input:is) | input == ',' = ' ' : checking is
               | input == '(' = checking is
               | input == ')' = checking is
               | otherwise = input : checking is

-- here is a linearly operating converter that converts IPA-Strings into a list of consonants, vowels and white spaces, it was precisely as annoying to code as it looks

parseIPA :: [Char] -> [IPA]
parseIPA [] = []
parseIPA (x:xs) | x `elem` cSym = case Map.lookup x cInventory of
      Just temp -> [IPAc temp] ++ parseIPA xs
      Nothing   -> [IPAc UnreadableCons] ++ parseIPA xs
                | x `elem` vSym = case Map.lookup x vInventory of
      Just temp -> [IPAv temp] ++ parseIPA xs
      Nothing   -> [IPAv UnreadableVowel] ++ parseIPA xs
                | x `elem` supSym = case Map.lookup x supInventory of
      Just temp -> [IPAm (Msup temp)] ++ parseIPA xs
      Nothing   -> [IPAm (Msup UnreadableSuprasegmental)] ++ parseIPA xs
                | x `elem` diaSym = case Map.lookup x diaInventory of
      Just temp -> [IPAm (Mdia temp)] ++ parseIPA xs
      Nothing   -> [IPAm (Mdia UnreadableDiacritic)] ++ parseIPA xs
                | x `elem` tonSym = case Map.lookup x tonInventory of
      Just temp -> [IPAm (Mton temp)] ++ parseIPA xs
      Nothing   -> [IPAm (Mton UnreadableTone)] ++ parseIPA xs
                | otherwise = case x of
      ' '       -> [IPAs Whitespace] ++ parseIPA xs


-- this is just for playing around, parses a String into a vertical list of IPA descriptions, doesn't yet support diacritics, suprasegmentals or tone because I was tired
parseIPA2 :: [Char] -> IO ()
parseIPA2 [] = putStr ""
parseIPA2 (x:xs) | x `elem` cSym = case Map.lookup x cInventory of
      Just temp -> do
        putStrLn $ show (IPAc temp)
        parseIPA2 xs
      Nothing   -> do
        putStrLn $ show (IPAc UnreadableCons)
        parseIPA2 xs
                | x `elem` vSym = case Map.lookup x vInventory of
      Just temp -> do
        putStrLn $ show (IPAv temp)
        parseIPA2 xs
      Nothing   -> do
        putStrLn $ show (IPAv UnreadableVowel)
        parseIPA2 xs
                | otherwise = case x of
      ' '       -> do
        putStrLn $ show (IPAs Whitespace)
        parseIPA2 xs

-- we need something to deal with whitespaces, morpheme boundaries (and probably worse ): ) I also derived it from Ord because we might care for a ranking of boundaries once I find it in me to abandon the not-too phonological concept of "whitespace" for sth cooler
data Symbol = Whitespace | MorphemeBoundary
  deriving (Eq, Show, Read, Ord)

-- here is the source data for all IPA symbols except data Symbol (name change pending), some functions may call xSym or xSpec, most should work with the much faster xInventory as it utilizes Data.Map
cInventory = Map.fromList $ tuplify cSym cSpec
vInventory = Map.fromList $ tuplify vSym vSpec
diaInventory = Map.fromList $ tuplify diaSym diaSpec
supInventory = Map.fromList $ tuplify supSym supSpec
tonInventory = Map.fromList $ tuplify tonSym tonSpec

diaSym = ['ʰ','ʷ','ʲ','ˠ','ˤ','ⁿ','ˡ']
diaSpec = [Aspirated, Labialized, Palatalized, Velarized, Pharyngealized, NasalRelease, LateralRelease]

supSym = ['ː','ˑ']
supSpec = [Long, HalfLong]

tonSym = ['˥','˦','˧','˨','˩']
tonSpec = [FallingTone, HighTone, MidTone, LowTone, RisingTone]

vSym = ['i','y','ɨ','ʉ','ɯ','u','ɪ','ʏ','ʊ','e','ø','ɘ','ɵ','ɤ','o','ə',
  'ɛ','œ','ɜ','ɞ','ʌ','ɔ','æ','ɐ','a','ɶ','ɑ','ɒ']

vSpec = [V Close Front Unrounded,
 V Close Front Rounded,
 V Close Central Unrounded,
 V Close Central Rounded,
 V Close Back Unrounded,
 V Close Back Rounded,
 V CloseCloseMid FrontCentral Unrounded,
 V CloseCloseMid FrontCentral Rounded,
 V CloseCloseMid BackCentral Rounded,
 V CloseMid Front Unrounded,
 V CloseMid Front Rounded,
 V CloseMid Central Unrounded,
 V CloseMid Central Rounded,
 V CloseMid Back Unrounded,
 V CloseMid Back Rounded,
 V Mid Central Unrounded,
 V OpenMid Front Unrounded,
 V OpenMid Front Rounded,
 V OpenMid Central Unrounded,
 V OpenMid Central Rounded,
 V OpenMid Back Unrounded,
 V OpenMid Back Rounded,
 V OpenOpenMid Front Unrounded,
 V OpenOpenMid Central Unrounded,
 V Open Front Unrounded,
 V Open Front Rounded,
 V Open Back Unrounded,
 V Open Back Rounded]

cSym = ['p','b','m','ʙ','ɸ','β','ɱ','ⱱ','f','v','ʋ','θ','ð','t','d','n','r','ɾ',
  's','z','ɬ','ɮ','ɹ','l','ʃ','ʒ','ʈ','ɖ','ɳ','ɽ','ʂ','ʐ','ɻ','ɭ','c','ɟ','ɲ','ç','ʝ',
  'j','ʎ','k','g','ŋ','x','ɣ','ɰ','ʟ','q','ɢ','ɴ','ʀ','χ','ʁ','ħ','ʕ','ʔ','h','ɦ']

cSpec = [C Bilabial Plosive Voiceless,
 C Bilabial Plosive Voiced,
 C Bilabial Nasal Voiced,
 C Bilabial Trill Voiced,
 C Bilabial Fricative Voiceless,
 C Bilabial Fricative Voiced,
 C Labiodental Nasal Voiced,
 C Labiodental Fricative Voiced,
 C Labiodental Fricative Voiceless,
 C Labiodental Tap Voiced,
 C Labiodental Approximant Voiced,
 C Dental Fricative Voiceless,
 C Dental Fricative Voiced,
 C Alveolar Plosive Voiceless,
 C Alveolar Plosive Voiced,
 C Alveolar Nasal Voiced,
 C Alveolar Trill Voiced,
 C Alveolar Tap Voiced,
 C Alveolar Fricative Voiceless,
 C Alveolar Fricative Voiced,
 C Alveolar Lateralfricative Voiceless,
 C Alveolar Lateralfricative Voiced,
 C Alveolar Approximant Voiced,
 C Alveolar Lateralapproximant Voiced,
 C Postalveolar Fricative Voiceless,
 C Postalveolar Fricative Voiced,
 C Retroflex Plosive Voiceless,
 C Retroflex Plosive Voiced,
 C Retroflex Nasal Voiceless,
 C Retroflex Tap Voiceless,
 C Retroflex Fricative Voiceless,
 C Retroflex Fricative Voiced,
 C Retroflex Approximant Voiced,
 C Retroflex Lateralapproximant Voiced,
 C Palatal Plosive Voiceless,
 C Palatal Plosive Voiced,
 C Palatal Nasal Voiced,
 C Palatal Fricative Voiceless,
 C Palatal Fricative Voiced,
 C Palatal Approximant Voiced,
 C Palatal Lateralapproximant Voiced,
 C Velar Plosive Voiceless,
 C Velar Plosive Voiced,
 C Velar Nasal Voiced,
 C Velar Fricative Voiceless,
 C Velar Fricative Voiced,
 C Velar Approximant Voiced,
 C Velar Lateralapproximant Voiced,
 C Uvular Plosive Voiceless,
 C Uvular Plosive Voiced,
 C Uvular Nasal Voiced,
 C Uvular Trill Voiced,
 C Uvular Fricative Voiceless,
 C Uvular Fricative Voiced,
 C Pharyngeal Fricative Voiceless,
 C Pharyngeal Fricative Voiced,
 C Glottal Plosive Voiceless,
 C Glottal Fricative Voiceless,
 C Glottal Fricative Voiced]

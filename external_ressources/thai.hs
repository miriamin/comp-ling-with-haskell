{- 
Thai
Silbeninitial 			silbenfinal 
f, p, b, pʰ	    		p̚
t͡ɕ, t͡ɕʰ, d, s, tʰ	    t̚
l, r, n                 n
k, g                    k̚
ŋ	                    ŋ
ʔ	                    ʔ / ʔ̚
w	                    w
j	                    j
m	                    m

Es gibt keine Silbeninitialen Vokale am Wortanfang (ʔ wird eingesetzt). nach kurzen Vokalen steht ein [ʔ] (wobei ich persönlich das eher als [ʔ̚] analysieren würde.

Aufgaben:

Formuliert phonologische Regeln für die Daten
Programmiert einen Akzeptor für dieses Phänomen
-}

-- meine erste Idee, abgebrochen, nachdem ich bemerkt habe, dass es sinnlos war. Kommt vielleicht eher einem Transduktor nach aber ich hab keine Ahnung
--convertInitialFinal laut | ((laut == "f") || (laut == "p") || (laut == "b") || (laut == "pʰ")) = "p̚"
--                         | ((laut == "t͡ɕ") || (laut == "t͡ɕʰ") || (laut == "d") || (laut == "s") || (laut == "tʰ")) = "t̚"

silbeninitial :: [Char]              
silbeninitial = "fpbpʰt͡ɕt͡ɕʰdstʰlrnkgŋʔwjm"

silbenfinal :: [Char]
silbenfinal = "p̚tnk̚ŋʔʔ̚wjm"

solo :: [Char]
solo = "ŋʔwjm"

-- sehr primitiver Ansatz, aber ich verstehe die Aufgabenstellung vielleicht auch nicht ganz
check :: [Char] -> Bool
check (x:xs) = if x `elem` silbeninitial && last xs `elem` silbenfinal then True else False

-- nun der Ansatz mit einem DFA
type DFA state symb = ([state], [symb], state, [state], (state, symb) -> state)

myMachine1 :: DFA Int Char
myMachine1 = ([0, 1, 2, 42], sigma2, 0, [2], deltaB)
  where
    sigma2 = silbeninitial ++ silbenfinal
    deltaB (0, v) | v `elem` solo = 2
    deltaB (0, x) | x `elem` silbeninitial = 1
    deltaB (0, _) = 42
    deltaB (1, y) | y `elem` silbenfinal = 2
    deltaB (1, _) = 1
    deltaB (2, z) | z `elem` silbenfinal = 2
    deltaB (2, _) = 1
    deltaB (42, _) = 42

-- Testergebnisse:
-- "pn" = True
-- "fuck̚" = True
-- "pʰhjsfhn" = True
-- "t͡ɕjjjn" = True
-- "x" ist false
-- "m" ist true (da es sowohl initial als auch final sein kann)
-- "nat͡ɕʰ" ist false
-- alles, dessen erster Buchstabe nicht in silbeninitial und dessen letzter Buchstabe nicht in silbenfinal ist, ist false
-- basierend auf der Annahme, dass die Quelldaten von Max vollständig alle silbeninitialen und silbenfinalen Laute des Thailändischen enthalten :)

-- die recognizes Funktion ist einfach aus Dfaeasy übernommen
recognizes :: Eq q => DFA q s -> [s] -> Bool
a@(qs, sigma, q0, fs, delta) `recognizes` input =
  deltaStar (q0, input) `elem` fs
  where
      deltaStar = uncurry $ foldl (curry delta)

import Dfsa

{-voiced s = ( s == 'g') || (s == 'd') || (s == 'b') 
minVoiced s = not . voiced
-}

voiced :: [Char]
voiced = "gdb"

type DFA state symb = ([state], [symb], state, [state], (state, symb) -> state)

recognizes :: Eq q => DFA q s -> [s] -> Bool
a@(qs, sigma, q0, fs, delta) `recognizes` input =
  deltaStar (q0, input) `elem` fs
  where
      deltaStar = uncurry $ foldl (curry delta)

-- 0 ist Start, 1 ist -sth, 2 ist +sth)
finalDevoicing :: DFA Int Char
finalDevoicing = ([0,1,2], s, 0, [1], d)
 where
  s = ['a'..'z']
  d (0, x) | x `elem` voiced = 2
           | otherwise = 1
  d (1, x) | x `elem` voiced = 2
           | otherwise = 1
  d (2, x) | x `elem` voiced = 2
           | otherwise = 1
           
{-testing:
finalDevoicing `recognizes`

 -} 
          
turkish :: [Char] -> Bool
turkish xs = finalDevoicing `Main.recognizes` xs                

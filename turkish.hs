import Dfsa

{-voiced s = ( s == 'g') || (s == 'd') || (s == 'b') 
minVoiced s = not . voiced
-}

voiced :: [Char]
voiced = "gdb"

-- works for both orthographic trukish and phonetic script 
unrounded :: [Char]
unrounded = "eiaıɯ"

rounded :: [Char]
rounded = "øöüyuo"

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
-- 0 is start, 1 is seen no vowel, 2 is seen -round vowel, 3 is seen round vowel, 99 is dead     
roundnessHarmony :: DFA Int Char
roundnessHarmony = ([0,1,2,3,99],s,0,[1,2,3],d)
 where
  s = ['a'..'z'] ++ rounded ++ unrounded ++ "-"
  d (0, x) | elem x unrounded = 2
           | elem x rounded = 3
           | otherwise = 1
  d (1, x) | elem x unrounded = 2
           | elem x rounded = 3
           | otherwise = 1    
  d (2,x)  | elem x unrounded = 2
           | elem x rounded = 99
           | otherwise = 2
  d (3,x)  | elem x unrounded = 99
           | elem x rounded = 3
           | otherwise = 3  
  d (99,x) = 99                           
           
{-testing:
finalDevoicing `recognizes`

 -} 
          
turkish :: [Char] -> Bool
turkish xs = finalDevoicing `Main.recognizes` xs                

turkish2 :: [Char] -> Bool
turkish2 xs = roundnessHarmony `Main.recognizes` xs   

voiced :: [Char]
voiced = "gdb"

-- works for both orthographic trukish and phonetic script
-- redundant for lazyness reasons 
alphabet :: [Char]
alphabet = ['a'..'z'] ++ unrounded ++ rounded ++ "-"

unrounded :: [Char]
unrounded = "eiaıɯ"

rounded :: [Char]
rounded = "øöüyuo"

vowels :: [Char]
vowels = rounded ++ unrounded

type DFA state symb = ([state], [symb], state, [state], (state, symb) -> state)

recognizes :: Eq q => DFA q s -> [s] -> Bool
a@(qs, sigma, q0, fs, delta) `recognizes` input =
  deltaStar (q0, input) `elem` fs
  where
      deltaStar = uncurry $ foldl (curry delta)

-- 0 ist Start, 1 ist -sth, 2 ist +sth)
finalDevoicing :: DFA Int Char
finalDevoicing = ([0,1,2], alphabet, 0, [1], d)
 where
  d (0, x) | x `elem` voiced = 2
           | otherwise = 1
  d (1, x) | x `elem` voiced = 2
           | otherwise = 1
  d (2, x) | x `elem` voiced = 2
           | otherwise = 1
-- 0 is start, 1 is seen no vowel, 2 is seen -round vowel, 3 is seen round vowel, 99 is dead     
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
  
  
-- two consonants never occur together at the beginning of native words (Lewis p 9)
-- 0 is start, 1 is vowel/good, 2 is initial Consonant, 99 is consonnat in position 2  
rmInitialCons :: DFA Int Char     
rmInitialCons = ([0,1,2,99],alphabet,0,[1],d)
 where
  d (0, x) | elem x vowels = 1
           | otherwise = 2
  d (1, x) = 1
  d (2, x) | elem x vowels = 2
           | otherwise = 99
  d (99, x) = 99     
           
{-testing:
finalDevoicing `recognizes`

 -} 


          
turkish :: [Char] -> Bool
turkish xs = finalDevoicing `Main.recognizes` xs 
          && roundnessHarmony `Main.recognizes` xs 
          && rmInitialCons `Main.recognizes` xs             

turkish2 :: [Char] -> Bool
turkish2 xs = roundnessHarmony `Main.recognizes` xs   

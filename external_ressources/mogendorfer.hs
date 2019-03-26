module MogendorferKauderwelsch where

import Dfst
import IPA2
import IPA_Test
import Data.Maybe
import Lomongo hiding (voicedStop, continuant)

-- dataset 1

data States = SeenLV | SeenNone

longVowel :: IPA -> Bool
longVowel v | Vowel `isTyping` v = if elem Long $ duration v then True else False
            | otherwise = False

voicedStop :: IPA -> Bool
voicedStop c | complexity c == Complex = False
             | Consonant `isTyping` c = if  ( elem Voiced $ uVoicing c ) && ( elem Plosive $ uManner c ) then True else False
             | otherwise = False

continuant :: IPA -> IPA 
continuant ipa@(IPA [ ((V o f r, airstream), modif) ]) = ipa
continuant     (IPA [ ((C p m v, airstream), modif) ]) = IPA [((C place' manner' v, airstream), modif)]
  where manner' | m == Plosive = Fricative
        place'  | p == Velar = Postalveolar

muinruff1 = mkForwardTransducer [SeenNone, SeenLV] alpha alpha SeenNone [] delta finalout
  where 
    alpha = test1 ++ parseIPA "g̥ ʒ̥ ʒ"
    finalout _ = Just []
    delta SeenNone x | longVowel x = Just (SeenLV, [x])
                     | otherwise = Just (SeenNone, [x])
    delta SeenLV x   | voicedStop x = Just (SeenNone, [continuant x])
                     | otherwise = Just (SeenNone, [x])

in1 = map parseIPA ["fliːgn", "ziːg̥", "kʰʁiːg̥", "lyːgn"]
out1 = map parseIPA ["fliːʒn", "ziːʒ̥", "kʰʁiːʒ̥", "lyːʒn"]

input1 = in1
output1 = out1

{- testing
*MogendorferKauderwelsch> (==) output1 $ map fromJust $ map (transduce muinruff1) input1
True
-}


-- dataset 2, voicedStop, longVowel and data States remain the same

continuant2 :: IPA -> IPA 
continuant2 ipa@(IPA [ ((V o f r, airstream), modif) ]) = ipa
continuant2     (IPA [ ((C p m v, airstream), modif) ]) = IPA [((C place' manner' v, airstream), modif)]
  where manner' | m == Plosive = Fricative
                | otherwise = m
        place'  | p == Velar = Postalveolar
                | p == Bilabial = Labiodental

muinruff2 = mkForwardTransducer [SeenNone, SeenLV] alpha alpha SeenNone [] delta finalout
  where 
    alpha = test1 ++ parseIPA "g̥ ʒ̥ ʒ t͡s"
    finalout _ = Just []
    delta SeenNone x | longVowel x = Just (SeenLV, [x])
                     | otherwise = Just (SeenNone, [x])
    delta SeenLV x   | voicedStop x = Just (SeenNone, [continuant2 x])
                     | otherwise = Just (SeenNone, [x])


in2 = map parseIPA ["ʁyːbɐ", "t͡sviːbl", "kyːbl"]
out2 = map parseIPA ["ʁyːvɐ", "t͡sviːvl", "kyːvl"]

input2 = in1 ++ in2
output2 = out1 ++ out2

{- testing
*MogendorferKauderwelsch> (==) output2 $ map fromJust $ map (transduce muinruff2) input2
True
-}


-- dataset 3, notice the changes in continuant

continuant3 :: IPA -> IPA 
continuant3 ipa@(IPA [ ((V o f r, airstream), modif) ]) = ipa
continuant3     (IPA [ ((C p m v, airstream), modif) ]) = IPA [((C place' manner' v, airstream), modif)]
  where manner' | m == Plosive = if p == Alveolar then Approximant else Fricative
                | otherwise = m
        place'  | p == Velar = Postalveolar
                | p == Bilabial = Labiodental
                | otherwise = p

muinruff3 = mkForwardTransducer [SeenNone, SeenLV] alpha alpha SeenNone [] delta finalout
  where 
    alpha = test1 ++ parseIPA "g̥ ʒ̥ ʒ t͡s ɹ"
    finalout _ = Just []
    delta SeenNone x | longVowel x = Just (SeenLV, [x])
                     | otherwise = Just (SeenNone, [x])
    delta SeenLV x   | voicedStop x = Just (SeenNone, [continuant3 x])
                     | otherwise = Just (SeenNone, [x])


in3 = map parseIPA ["viːdɐ", "liːdɐ"]
out3 = map parseIPA ["viːɹɐ", "liːɹɐ"]

input3 = in1 ++ in2 ++ in3
output3 = out1 ++ out2 ++ out3

{- testing
*MogendorferKauderwelsch> (==) output3 $ map fromJust $ map (transduce muinruff3) input3
True
-}


-- dataset 4
{-
there are two different ways to go about this from the data offered. We could analyse this
as one singular effect and we'd need to write a single transducer (a). It could also be the case
that there is a general constraint that forbids long (well at least close front) vowels before 
voiced continuants (b) which I personally think is the case but both interpretations are 
interesting for the sake of this exercise.

We also need to change the input-output because the data from set 2 is being revised
-}

-- (a), note how I changed the names of all data types to avoid naming collisions with data States

data States2 = SeenLV2 IPA | SeenNone2

shorten :: IPA -> IPA 
shorten v@(IPA [ (s, modif) ]) = if elem Long $ duration v 
    then IPA [(s, modif')] else v
  where modif' = filter (Long /=) modif

midcentralize :: IPA -> IPA
midcentralize (IPA [ ((V o f r, airstream), modif) ]) = (IPA [ ((V o' f' r, airstream), modif) ])
  where 
    (o',f') = case id (o, f) of 
          (Close, Front) -> (CloseCloseMid, FrontCentral)
          (Close, Back)  -> (CloseCloseMid, BackCentral)
          (CloseMid, Front) -> (OpenMid, Front)
          (CloseMid, Back) -> (OpenMid, Back)
          (x,y) -> (x,y)

adjustV :: IPA -> IPA
adjustV = shorten . midcentralize

muinruff4 = mkForwardTransducer states alpha alpha SeenNone2 [] delta finalout
  where 
    alpha = test1 ++ parseIPA "g̥ ʒ̥ ʒ t͡s ɹ"
    states = SeenNone2 : [SeenLV2 ipa | ipa <- alpha, (Vowel `isTyping` ipa) && (elem Long $ duration ipa)]
    finalout (SeenLV2 v) = Just [v]
    finalout _ = Just []
    delta SeenNone2 x    | longVowel x = Just (SeenLV2 x, [])
                         | otherwise = Just (SeenNone2, [x])
    delta (SeenLV2 v) x  | voicedStop x = 
        if elem Bilabial $ uPlace x then Just (SeenNone2, [adjustV v, continuant3 x]) 
          else Just (SeenNone2, [v, continuant3 x])
                         | otherwise = Just (SeenNone2, [v, x])


in4 = map parseIPA ["ʁyːbɐ", "t͡sviːbl", "kyːbl"]
out4 = map parseIPA ["ʁʏvɐ", "t͡svɪvl", "kʏvl"]

input4 = in1 ++ in3 ++ in4
output4 = out1 ++ out3 ++ out4

{- testing
*MogendorferKauderwelsch> (==) output4 $ map fromJust $ map (transduce muinruff4) input4
True
-}

-- (b) using muinruff3 and data States2, muinruff5 can be implemented forward (fw) or backward (bw)

voicedContinuant :: IPA -> Bool
voicedContinuant c | Consonant `isTyping` c = if  ( elem Voiced $ uVoicing c ) && ( elem Fricative $ uManner c ) || (elem Approximant $ uManner c) then True else False
                   | otherwise = True


-- forward transducer
muinruff5_b_fw = mkForwardTransducer states alpha alpha SeenNone2 [] delta finalout
  where
    alpha = test1 ++ parseIPA "g̥ ʒ̥ ʒ t͡s ɹ"
    states = SeenNone2 : [SeenLV2 ipa | ipa <- alpha, (Vowel `isTyping` ipa) && (elem Long $ duration ipa)]
    finalout (SeenLV2 v) = Just [v]
    finalout _ = Just []
    delta SeenNone2 x    | longVowel x = Just (SeenLV2 x, [])
                         | otherwise = Just (SeenNone2, [x])
    delta (SeenLV2 v) x  | voicedContinuant x = 
        if elem Labiodental $ uPlace x then Just (SeenNone2, [adjustV v, x]) 
          else Just (SeenNone2, [v, x])
                         | otherwise = Just (SeenNone2, [v, x])


in5_b = map parseIPA ["ʁyːvɐ", "t͡sviːvl", "kyːvl"] -- this is edited!
out5_b = map parseIPA ["ʁʏvɐ", "t͡svɪvl", "kʏvl"]

{- intermediary testing
*MogendorferKauderwelsch> (==) out5_b $ map fromJust $ map (transduce muinruff5_b_fw) in5_b
True
-}

-- composite testing

-- bullshit composite testing
fsts = map parseIPA ["ʁyːbɐ", "t͡sviːbl", "kyːbl"]
snds = in5_b
trds = out5_b

{- 

step 1
*MogendorferKauderwelsch> (==) snds $ map fromJust $ map (transduce muinruff3) fsts
True

step 2
*MogendorferKauderwelsch> (==) trds $ map fromJust $ map (transduce muinruff5_b_fw) snds
True

step 1 & 2 after one another
*MogendorferKauderwelsch> (==) trds $ map fromJust $ map (transduce muinruff5_b_fw) $ map fromJust $ map (transduce muinruff3) fsts
True 

-}


muinruff5_fw = muinruff3 `compose` muinruff5_b_fw

-- this code produces a file
main5_fw :: IO ()
main5_fw = do 
  sequence_ $ fmap (\t -> test t testData) muinruff5_fw
  return ()
    where
      testData = zip in4 out4



-- backward transducer
{-
I did not know how to make this work at all without changing the Dfst module since there is no
specification for transducing backwards.
If you want to do what I did, add this line after the definition of transduce
transduce t@(S Backward _ _ _ _ _ _ _) bs = transduce (reverseDirection t) $ reverse bs

However, the result still needs to be reversed. See 'intermediary testing' for an example. In
that I use 'map' because the input is a list of strings

-}


data States3 = SeenCont3 | SeenNone3

muinruff5_b_bw = mkBackwardTransducer [SeenCont3, SeenNone3] alpha alpha SeenNone3 [] delta finalout
  where
    alpha = test1 ++ parseIPA "g̥ ʒ̥ ʒ t͡s ɹ"
    finalout _ = Just []
    delta SeenNone3 x    | Vowel `isTyping` x = Just (SeenNone3, [x])
                         | voicedContinuant x = 
        if elem Labiodental $ uPlace x then Just (SeenCont3, [x])
          else Just (SeenNone3, [x])
                         | otherwise = Just (SeenNone3, [x])
    delta SeenCont3 x    | Vowel `isTyping` x = 
        if elem Long $ duration x then Just (SeenNone3, [adjustV x]) 
          else Just (SeenNone3, [x])
                         | otherwise = Just (SeenNone3, [x])

-- in5_b already defined
-- out5_b already defined

{- intermediary testing
*MogendorferKauderwelsch> (==) out5_b $ map reverse $  map fromJust $ map (transduce muinruff5_b_bw) in5_b
True
-}

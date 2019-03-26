module Dfa 


where


-- type synonym of a DFA as a 5-tuple consisting of a list of states, a list of symbols, a start state, a list of final states and a (partial) function mapping state-symbol pairs to states
type DFA state symb = ([state], [symb], state, [state], (state, symb) -> state)


-- these are handy functions to access the parts of our DFA using pattern matching
states :: DFA state symbol -> [state]
states (allstates, _, _, _, _) = allstates

sigma :: DFA state symbol -> [symbol]
sigma (_, alphabet, _, _, _) = alphabet

start :: DFA state symbol -> state
start (_, _, startstate, _, _) = startstate

finals :: DFA state symbol -> [state]
finals (_, _, _, finalstates, _) = finalstates

delta :: DFA state symbol -> (state, symbol) -> state
delta (_, _, _, _, deltafunction) = deltafunction



-- a constraint against 'b' in all input strings over Sigma = {a,b,c} (formerly list of symbols - can be understood as our alphabet)
myMachine1 :: DFA Int Char
myMachine1 = ([0,1], ['a','b','c'], 0, [0], deltaB)
  where
    deltaB (0, 'b') = 1
    deltaB (0, _)   = 0
    deltaB (1, _)   = 1

{- this is our very important recognizer function, it can talk to the machine and make it do stuff
1. in the function definition the name is written in ``s in between it's two arguments, please don't be confused by this I just try to stick to the original code 
2. in the definition of deltaStar the argument to the function is omitted for the input tuple would be written after the term (compare this to the foldl-alternative below): deltaStar (q, input) = uncurry $ foldl (curry delta) (q, input)
2.1 except I sort of lied in 2 because if you actually wrote it that way, Haskell would think you want to apply foldl (curry delta) to (q, input) before you uncurry it which of course doesn't work as we just said
2.2 yes, I also think this is kinda hard to read if you leave it out but it sure makes you look smart if you can casually understand it
3. so deltastar takes a tuple of a start state and an input list, lets delta take the start state and the first element of the input which returns another state that is then again fed into delta with the second element of the input list (and so on #ThisIsHowFoldlWorks) and the final result is then tested for wether or not it is a final state (`elem` fs) - this returns a Boolean value and we get our result from the typing of the function in the first line
-}
recognizes :: Eq q => DFA q s -> [s] -> Bool
a@(qs, sigma, q0, fs, delta) `recognizes` input =
  deltaStar (q0, input) `elem` fs
  where
    deltaStar = uncurry $ foldl (curry delta)

{- some alternative versions of delta* in 'recognizes'

using recursion
deltaStar (q, []) = q
deltaStar (q, (b:bs)) = deltaStar ((delta (q,b)), bs)

using foldl, stating the argument of the function: (q, input)
'delta' needs to be curried because we originally defined it as dealing with tuples (so two arguments at once) but 'foldl' requires a functio
deltaStar (q, input) = foldl (curry delta) a input

-}

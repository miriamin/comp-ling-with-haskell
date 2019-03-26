-- Aufgabe: Akzeptor der Auslautverhärtung überprüft

import IPS
import dfaeasy

auslautVH :: DFA String Int 
auslautVH = A qs sigma q0 fs d
	where
		sigma = cSym ++ vSym ++ supSym ++ diasym ++ ton ++ [" "]
		qs = ["start", "lastSound", "lastSoundVowel", "lastSoundConsonant", "lastSoundVoiced", "lastSoundUnvoiced", "lastSoundObstruent", "lastSoundNotObstruent"]
		




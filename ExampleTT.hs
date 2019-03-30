import DTT

acceptorRules =
  [(('a',[]),False) -- a leaf 'a' has no bs
  , (('b',[]),True) -- a leaf 'b' has exactly one b
  , (('f',[False,False]),False) -- a tree with b-less daughters has no b
  , (('f',[False,True]),True) -- a tree with a b-less left daughter, and a b-full right daughter has exactly one b
  , (('f',[True,False]),True) -- a tree with a b-full left daughter, and a b-less right daughter has exactly one b
  ]

acceptor = mkAcceptor acceptorRules [True]


evtlTest :: BUTA Bool Char
evtlTest = mkBUTA acceptorRules [True] 



leafA = NodeC 'a' []
leafC = NodeC 'c' []
intF = NodeC 'f' [Hole 0, Hole 1]
moveB = NodeC 'f' [NodeC 'b' [], intF]

buRules =
  [(('a',[]),(Nothing,leafA)) 
  , (('b',[]),(Just 0, leafC)) 
  , (('f',[Nothing,Nothing]),(Nothing,intF))
  , (('f',[Nothing,Just 0]),(Just 1,intF)) 
  , (('f',[Nothing,Just 1]),(Just 2,intF)) 
  , (('f',[Nothing,Just 2]),(Nothing,moveB)) 
  , (('f',[Just 0,Nothing]),(Just 1,intF)) 
  , (('f',[Just 1,Nothing]),(Just 2,intF)) 
  , (('f',[Just 2,Nothing]),(Nothing,moveB)) 
  ]

buTrans = mkBUTrans buRules [Nothing]

mkTree :: String -> Tree Char
mkTree w = Node '$' [foldr (\a t -> Node a [t]) (Node '#' []) w]

hash = CHole0 0
dollar = CHole1 0 [NodeCC 'c' []]
aCC =  NodeCC 'f' [NodeCC 'a' [],CHole1 0 [NodeCC 'f' [CHole0 0, NodeCC 'a' []]]]
bCC =  NodeCC 'f' [NodeCC 'b' [],CHole1 0 [NodeCC 'f' [CHole0 0, NodeCC 'b' []]]]

macroRules =
  [(('#',[]),((),hash)) 
  , (('a',[()]),((),aCC)) 
  , (('b',[()]),((),bCC)) 
  , (('$',[()]),((),dollar))
  ]

macroTrans = mkMacroTrans macroRules [()]

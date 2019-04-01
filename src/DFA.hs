module DFA
    ( DFA(DFA)
    , run
    )
where

data DFA st  = DFA
    { states :: [st]
    , sigma :: [Char]
    , delta :: (st -> Char ->  st)
    , startState :: st
    , acceptStates :: [st]
    }

run :: Eq st => String -> DFA st -> Bool
run word dfa = foldl (delta dfa) (startState dfa) word `elem` acceptStates dfa


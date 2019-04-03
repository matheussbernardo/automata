module DFA
    ( DFA(DFA)
    , run
    )
where

data DFA st  = DFA
    { states :: [st]
    , sigma :: [String]
    , delta :: (st -> Char ->  st)
    , startState :: st
    , acceptStates :: [st]
    }

instance (Show st) => Show (DFA st) where
    show (DFA states sigma delta start accept) =
        show states ++ show sigma ++ show start ++ show accept

run :: Eq st => String -> DFA st -> Bool
run word dfa = foldl (delta dfa) (startState dfa) word `elem` acceptStates dfa


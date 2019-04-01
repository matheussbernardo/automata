module NFA
  ( NFA(NFA)
  , run
  , epsilonClosure
  )
where

import qualified Data.Set                      as S


data NFA st  = NFA
    { states :: S.Set st
    , sigma :: [Char]
    , delta :: (st -> Char -> S.Set st)
    , startState :: st
    , acceptStates :: S.Set st
    }

run :: Ord st => String -> NFA st -> Bool
run word nfa = not $ S.null $ S.intersection (run' word) (acceptStates nfa)
 where
  run' = foldl extendedDelta' initialStates
  extendedDelta' states char = extendedDelta states char nfa
  initialStates = epsilonClosure (S.singleton $ startState nfa) nfa

extendedDelta :: Ord st => S.Set st -> Char -> NFA st -> S.Set st
extendedDelta sts char nfa = S.foldr appendStates S.empty sts
 where
  appendStates s acc = S.union acc $ move s
  move s = epsilonClosure (delta nfa s char) nfa

epsilonClosure :: Ord st => S.Set st -> NFA st -> S.Set st
epsilonClosure states nfa = S.foldr move states states
  where move state acc = S.union acc (delta nfa state 'E')

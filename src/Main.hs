module Main where

import           DFA
import           NFA                            ( NFA(..)
                                                , run
                                                )
import qualified Data.Set                      as S

main :: IO ()
main = do
    print (DFA.run "BABABA" exampleDFA)
    print (NFA.run "A" exampleNFA)


step' :: Integer -> Char -> Integer
step' 0 'A' = 0
step' 0 'B' = 1
step' 1 'A' = 2
step' 1 'B' = 0
step' 2 'A' = 1
step' 2 'B' = 2

exampleDFA :: DFA Integer
exampleDFA = DFA [0, 1, 2] ['A', 'B'] step' 0 [0]

exampleDelta :: Integer -> Char -> S.Set Integer
exampleDelta 0 'A' = S.fromList []
exampleDelta 1 'A' = S.fromList [2]
exampleDelta 2 'A' = S.fromList [3]
exampleDelta 3 'A' = S.fromList []
exampleDelta 4 'A' = S.fromList []
exampleDelta 5 'A' = S.fromList []
exampleDelta 0 'E' = S.fromList [0, 1, 4]
exampleDelta 1 'E' = S.fromList [1]
exampleDelta 2 'E' = S.fromList [2, 3]
exampleDelta 3 'E' = S.fromList [3]
exampleDelta 4 'E' = S.fromList [4]
exampleDelta 5 'E' = S.fromList [5]
exampleDelta 0 'B' = S.fromList []
exampleDelta 1 'B' = S.fromList []
exampleDelta 2 'B' = S.fromList []
exampleDelta 3 'B' = S.fromList []
exampleDelta 4 'B' = S.fromList [5]
exampleDelta 5 'B' = S.fromList []


exampleNFA :: NFA Integer
exampleNFA = NFA (S.fromList [0, 1, 2, 3, 4, 5])
                 ['A', 'B']
                 exampleDelta
                 0
                 (S.fromList [3, 5])

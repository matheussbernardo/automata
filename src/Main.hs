module Main where

import           DFA
import           NFA                            ( NFA(..)
                                                , run
                                                )
import qualified Data.Set                      as S
import qualified Data.Map                      as Map
import           Data.Maybe
import           System.Environment

main :: IO ()
main = do
  filename            <- head <$> getArgs
  automatoDescription <- lines <$> readFile filename
  let automato = constructAutomato automatoDescription
  let word     = last automatoDescription

  print (DFA.run word automato)

constructAutomato :: [String] -> DFA Int
constructAutomato input = DFA [0 .. (states - 1)]
                              symbols
                              transitions
                              initial
                              accept
 where
  states  = read $ input !! 0 :: Int
  size    = read $ take 1 (input !! 1) :: Int
  symbols = words $ drop 1 (input !! 1)
  transitions :: Int -> Char -> Int
  transitions state symbol = fromMaybe
    (-1)
    (lookup (state, symbol)
            (map transform ((take (states * size)) $ drop 2 input))
    )

  initial = read $ head $ drop (2 + (states * size)) input :: Int
  accept  = map read (words $ head $ drop (3 + (states * size)) input)

transform :: String -> ((Int, Char), Int)
transform transition = ((state, symbol), dest)
 where
  arr    = words $ transition
  state  = read $ arr !! 0
  symbol = head $ arr !! 1
  dest   = read $ arr !! 2

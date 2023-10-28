module Main (main) where

import Lib

main :: IO ()
main = do
  --doCmd (runInputT defaultSettings cli) (makeBasis [cardS, cardK], [])
  pure ()


testNamed :: NamedCards
testNamed = makeBasis [cardS, cardK]

testDeck :: Deck
testDeck = [Basic 'x', Group [Named "S", Named "K", Named "K", Basic 'x'], Basic 'y', Group [Named "S", Group [Named "S", Named "K", Named "K", Named "K"], Named "K", Basic 'x'], Basic 'z']
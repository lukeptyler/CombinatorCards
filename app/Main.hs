module Main (main) where

import Cards
import CLI

main :: IO ()
main = runCLI testNamed testDeck

testNamed :: NamedCards
testNamed = makeBasis [cardS, cardK]

testDeck :: Deck
testDeck = [Group [Named "S", Named "K", Named "K", Basic 'x'], Basic 'y', Group [Named "S", Group [Named "S", Named "K", Named "K", Named "K"], Named "K", Basic 'x'], Basic 'z']


















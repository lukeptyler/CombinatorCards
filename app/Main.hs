module Main (main) where

import Lib

import System.Console.Haskeline

import Control.Monad.Trans.Class

main :: IO ()
main = do
  runInputT defaultSettings $ doCmd cli testNamed testDeck
  pure ()

type CommandCLI = Command (InputT IO)

cli :: CommandCLI ()
cli = do
  deck <- getDeck
  lift $ outputStrLn $ "Starting deck: " ++ showDeck deck
  loop
  where
    loop :: CommandCLI ()
    loop = do
      played <- playNextPlayableCard
      deck' <- getDeck
      if played
      then do
        lift $ outputStrLn $ showDeck deck'
        lift $ waitForAnyKey ""
        loop
      else do
        lift $ outputStrLn "Fully simplified"
        lift $ outputStrLn $ "Final deck: " ++ showDeck deck'

playNextPlayableCard :: CommandCLI Bool
playNextPlayableCard = do
  topPlayed <- playTop

  groupPlayed <- onlyIf (not topPlayed) False $ do
    deck <- getDeck
    playNextGroup [] deck

  pure $ topPlayed || groupPlayed

playNextGroup :: Deck -> Deck -> CommandCLI Bool
playNextGroup before [] = do
  setDeck before
  pure False
playNextGroup before (Group grp : rest) = do
  setDeck grp
  played <- playNextPlayableCard

  if played
  then do
    grp' <- getDeck
    setDeck $ before ++ (if length grp' == 1 then grp' else [Group grp']) ++ rest
    pure True
  else playNextGroup (before ++ [Group grp]) rest
playNextGroup before (card : rest) = playNextGroup (before ++ [card]) rest


testNamed :: NamedCards
testNamed = makeBasis [cardS, cardK]

testDeck :: Deck
testDeck = [Group [Named "S", Named "K", Named "K", Basic 'x'], Basic 'y', Group [Named "S", Group [Named "S", Named "K", Named "K", Named "K"], Named "K", Basic 'x'], Basic 'z']


















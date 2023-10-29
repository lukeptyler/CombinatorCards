module CLI
  ( runCLI
  ) where

import Cards
import Command
import Parse

import System.Console.Haskeline

type CommandCLI = Command (InputT IO)

runCLI :: NamedCards -> Deck -> IO ()
runCLI named deck = runInputT defaultSettings $ doCmd cli named deck

cli :: CommandCLI ()
cli = simplifyCurrentDeck

simplifyCurrentDeck :: CommandCLI ()
simplifyCurrentDeck = do
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
        _ <- lift $ waitForAnyKey ""
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













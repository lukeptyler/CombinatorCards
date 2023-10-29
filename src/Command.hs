module Command 
  ( lift

  , Command, CommandIO
  , doCmd

  , onlyIf_, onlyIf

  , getInfo, getNamed, getDeck
  , setDeck

  , isPlayable
  , drawCard
  , playTop
  ) where

import Cards

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State

type Info = (NamedCards, Deck)
type Command = StateT Info

doCmd :: Monad m => Command m a -> NamedCards -> Deck -> m a
doCmd cmd named deck = evalStateT cmd (named, deck)

onlyIf_ :: Monad m => Bool -> Command m () -> Command m ()
onlyIf_ True  cmd = cmd
onlyIf_ False _   = pure ()

onlyIf :: Monad m => Bool -> a -> Command m a -> Command m a
onlyIf  True  _   cmd = cmd
onlyIf  False def _   = pure def

getInfo :: Monad m => Command m Info
getInfo = get

getNamed :: Monad m => Command m NamedCards
getNamed = gets fst

getDeck :: Monad m => Command m Deck
getDeck = gets snd

isPlayable :: Monad m => Command m Bool
isPlayable = do
  (named, deck) <- getInfo
  case deck of
    []                -> pure False
    (Basic _:_)       -> pure False
    (Named name:rest) -> pure $ isNamePlayable named name rest
    (Group _:_)       -> pure True
    (Var _:_)         -> error "Var should not be here" -- TODO: Better error messages

drawCard :: Monad m => Command m (Card, Deck)
drawCard = do
  deck <- getDeck
  pure (head deck, tail deck)

setDeck :: Monad m => Deck -> Command m ()
setDeck deck = modify $ \(named,_) -> (named,deck)

----------------------------------------------

-- play the top card. equals if top card was played
-- TODO: There must be a better way to do this
playTop :: Monad m => Command m Bool
playTop = do
  playable <- isPlayable
  onlyIf_ playable $ do
    named <- getNamed
    (top, deck') <- drawCard
    setDeck (playCard named top deck')
  pure playable

-- play top card until fully simplified starting at top and then in groups
runDeck :: Monad m => Command m Deck
runDeck = do
  runTop
  deck <- getDeck
  deck' <- mapM runGroup deck
  setDeck deck'
  pure deck'
  where
    runTop :: Monad m => Command m ()
    runTop = do
      playedCard <- playTop
      if playedCard
      then runTop
      else pure ()

    runGroup :: Monad m => Card -> Command m Card
    runGroup (Group grp) = do
      setDeck grp
      _ <- runDeck
      grp' <- getDeck
      if length grp' == 1
      then pure $ head grp'
      else pure $ Group grp'
    runGroup card = pure card


type CommandIO = Command IO

-- same as simplifyDeck showing intermediate steps
runDeckInteract :: (Deck -> IO ()) -> CommandIO ()
runDeckInteract draw = do
  lift $ putStrLn "Simplifying deck"
  drawDeck
  runTop
  deck <- getDeck
  deck' <- mapM runGroup deck
  setDeck deck'
  lift $ putStrLn "Deck simplified"
  drawDeck
  where
    drawDeck :: CommandIO ()
    drawDeck = do
      deck <- getDeck
      lift $ draw deck
      _ <- lift getLine
      pure ()

    runTop :: CommandIO ()
    runTop = do
      deck <- getDeck
      playedCard <- playTop
      if playedCard
      then do
        lift $ putStrLn $ "Played card " ++ show (head deck)
        drawDeck
        runTop
      else pure ()

    runGroup :: Card -> CommandIO Card
    runGroup (Group grp) = do
      lift $ putStrLn $ "Entering group (" ++ showDeck grp ++ ")"
      setDeck grp
      runDeckInteract draw
      grp' <- getDeck
      if length grp' == 1
      then pure $ head grp'
      else pure $ Group grp'
    runGroup card = pure card








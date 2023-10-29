module Lib where

import qualified Data.Map as M
import Data.List (intercalate)

import Control.Monad.IO.Class
import Control.Monad.Trans.State

-- Cards --
type Name = String

data Card
  = Basic Char    -- cards that do nothing
  | Named Name    -- cards with defined use
  | Group Deck    -- unnamed groups
  | Var Int       -- used for actions
instance Show Card where
  show (Basic c) = [c]
  show (Named n) = n
  show (Group g) = "(" ++ showDeck g ++ ")"
  show (Var n)   = "_" ++ show n ++ "_"

showDeck :: Deck -> String
showDeck deck = intercalate " " $ map show deck

type Deck = [Card]  -- ordered set of cards
type Action = (Int, Deck)  -- defined action (# vards, deck with vars)

type NamedCards = M.Map Name (Either Action Deck)  -- TODO: use Map or something

isNamePlayable :: NamedCards -> Name -> Deck -> Bool
isNamePlayable named name deck
  | M.notMember name named = False
  | otherwise = case named M.! name of
      Left (vars, _) -> vars <= length deck
      Right _        -> True

-- Assumes card is playable, will error or incorrect if unplayable
playCard :: NamedCards -> Card -> Deck -> Deck
playCard named (Named name) deck = case named M.!? name of
  Nothing          -> error $ show name ++ ": Name not known"
  Just (Right grp) -> grp ++ deck
  Just (Left (vars, result)) -> applyAction (M.fromList $ zip [1..vars] deck) result ++ drop vars deck
playCard _     (Group grp)  deck = grp ++ deck
playCard _     card         _    = error $ show card ++ ": Card not playable" -- TODO: better error messages

applyAction :: M.Map Int Card -> Deck -> Deck
applyAction _ [] = []
applyAction binding (Group grp:rest) = Group (applyAction binding grp) : applyAction binding rest
applyAction binding (Var v:rest) = case binding M.!? v of
  Nothing -> error $ show v ++ ": unbound var"
  Just card -> card : applyAction binding rest
applyAction binding (card:rest) = card : applyAction binding rest


-- Execution --
type Info = (NamedCards, Deck)
type Command = StateT Info

type CommandIO = Command IO

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
      runDeck
      grp' <- getDeck
      if length grp' == 1
      then pure $ head grp'
      else pure $ Group grp'
    runGroup card = pure card

-- same as simplifyDeck showing intermediate steps
runDeckInteract :: (Deck -> IO ()) -> CommandIO ()
runDeckInteract draw = do
  liftIO $ putStrLn "Simplifying deck"
  drawDeck
  runTop
  deck <- getDeck
  deck' <- mapM runGroup deck
  setDeck deck'
  liftIO $ putStrLn "Deck simplified"
  drawDeck
  where
    drawDeck :: CommandIO ()
    drawDeck = do
      deck <- getDeck
      liftIO $ draw deck
      _ <- liftIO getLine
      pure ()

    runTop :: CommandIO ()
    runTop = do
      deck <- getDeck
      playedCard <- playTop
      if playedCard
      then do
        liftIO $ putStrLn $ "Played card " ++ show (head deck)
        drawDeck
        runTop
      else pure ()

    runGroup :: Card -> CommandIO Card
    runGroup (Group grp) = do
      liftIO $ putStrLn $ "Entering group (" ++ showDeck grp ++ ")"
      setDeck grp
      runDeckInteract draw
      grp' <- getDeck
      if length grp' == 1
      then pure $ head grp'
      else pure $ Group grp'
    runGroup card = pure card

------------- Default Cards --------------

type DefaultCard = (Name, Either Action Deck)

makeBasis :: [DefaultCard] -> NamedCards
makeBasis = M.fromList

cardS :: DefaultCard
cardS = ("S", Left (3, [Var 1, Var 3, Group [Var 2, Var 3]]))

cardK :: DefaultCard
cardK = ("K", Left (2, [Var 1]))






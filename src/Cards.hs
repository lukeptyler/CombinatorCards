module Cards 
  ( Name
  , Card(..), Deck
  , Action, NamedCards

  , showDeck
  , isNamePlayable
  , playCard

  , DefaultCard
  , makeBasis
  , cardS, cardK
  ) where

import qualified Data.Map as M
import Data.List (intercalate)

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

type NamedCards = M.Map Name (Either Action Deck)

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

------------- Default Cards --------------

type DefaultCard = (Name, Either Action Deck)

makeBasis :: [DefaultCard] -> NamedCards
makeBasis = M.fromList

cardS :: DefaultCard
cardS = ("S", Left (3, [Var 1, Var 3, Group [Var 2, Var 3]]))

cardK :: DefaultCard
cardK = ("K", Left (2, [Var 1]))









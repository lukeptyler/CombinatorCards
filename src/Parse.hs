module Parse 
  ( parseDeck
  ) where

import Cards

import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator

type DeckParser = ParsecT String Int Identity

endOfToken :: DeckParser ()
endOfToken = lookAhead $ choice
  [ char ')' >> pure ()
  , space >> pure ()
  , eof
  ]

basicCard :: DeckParser Card
basicCard = do
  c <- lower
  endOfToken
  pure $ Basic c

namedCard :: DeckParser Card
namedCard = do
  c <- upper <|> (lower <* notFollowedBy endOfToken)
  cs <- many $ upper <|> lower <|> digit
  endOfToken
  pure $ Named (c:cs)

varCard :: DeckParser Card
varCard = do
  d <- oneOf "123456789"
  ds <- many digit
  let x = read (d:ds)
  endOfToken

  maxVar <- getState
  if x > maxVar
  then putState x
  else pure ()

  pure $ Var x

groupCard :: DeckParser Card
groupCard = do
  grp <- between (char '(') (char ')') cardList
  pure $ Group grp

cardList :: DeckParser [Card]
cardList = sepBy1 (choice 
  [ try basicCard
  , namedCard
  , groupCard
  , varCard
  ]) space

deckP :: DeckParser Deck
deckP = do
  deck <- cardList
  eof
  pure deck

parseDeck :: String -> Either ParseError (Deck, Int)
parseDeck = runParser (do
  deck <- deckP
  highestVar <- getState
  pure (deck, highestVar)
  ) 0 ""












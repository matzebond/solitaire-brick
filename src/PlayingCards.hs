{-# LANGUAGE TemplateHaskell #-}
module PlayingCards where

import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (.~), (%~), (^.))
import Data.List (sortBy)
import Data.Function  (on)
import System.Random

data Color = Red | Black
           deriving (Eq, Enum)
data Suit = Diamond | Heart | Spade | Club
          deriving (Eq, Enum, Show)
data Face = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
          deriving (Eq, Enum, Ord, Show)
data Card = Card { _suit :: Suit,
                   _face :: Face}
          deriving (Eq, Show)

makeLenses ''Card

colorSuit :: Suit -> Color
colorSuit Diamond = Red
colorSuit Heart = Red
colorSuit _ = Black

colorCard :: Card -> Color
colorCard (Card s _) = colorSuit s

type Deck = [Card]

allSuits :: [Suit]
allSuits = [Diamond, Heart, Spade, Club]

allFaces :: [Face]
allFaces = [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]

fullDeck :: Deck
fullDeck = map (uncurry Card) ((,) <$> allSuits <*> allFaces)
  -- alternative
  -- fullDeck = [Card s f | s <- allSuits, f <- allFaces]

class Abbrev a where
  abbrev :: a -> String

instance Abbrev Suit where
  abbrev Diamond = "d"
  abbrev Heart = "h"
  abbrev Spade = "s"
  abbrev Club = "c"

instance Abbrev Face where
  abbrev Ace = "A"
  abbrev Two = "2"
  abbrev Three = "3"
  abbrev Four = "4"
  abbrev Five = "5"
  abbrev Six = "6"
  abbrev Seven = "7"
  abbrev Eight = "8"
  abbrev Nine = "9"
  abbrev Ten = "T"
  abbrev Jack = "J"
  abbrev Queen = "Q"
  abbrev King = "K"

instance Abbrev Card where
  abbrev (Card s f) = abbrev f ++ abbrev s

class Get a where
  get :: String -> a

instance Get Suit where
  get "d" = Diamond
  get "h" = Heart
  get "s" = Spade
  get "c" = Club

instance Get Face where
  get "A" = Ace
  get "2" = Two
  get "3" = Three
  get "4" = Four
  get "5" = Five
  get "6" = Six
  get "7" = Seven
  get "8" = Eight
  get "9" = Nine
  get "T" = Ten
  get "J" = Jack
  get "Q" = Queen
  get "K" = King

instance Get Card where
  get [face,suit] = Card (get [suit]) (get [face])


shuffle :: StdGen -> [a] -> [a]
shuffle gen xs = map fst $ sortBy (compare `on` snd) zipped
  where zipped = zip xs (randoms gen :: [Int])

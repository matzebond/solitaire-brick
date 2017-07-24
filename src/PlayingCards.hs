module PlayingCards where

import System.Random
import Data.List (sortBy)
import Data.Function  (on)

data Color = Red | Black deriving (Eq, Show, Enum)
data Suit = Diamond | Heart | Spade | Club deriving (Eq, Show, Enum)
data Face = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Eq, Show, Enum, Ord)
data Card = Card { suit :: Suit,
                   face :: Face}
          deriving (Eq)

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

instance Show Card where
  show (Card s f) = show f ++ " of " ++ show s ++ "s"
  --show c = short c

class Short a where
  short :: a -> String

instance Short Suit where
  short Diamond = "d"
  short Heart = "h"
  short Spade = "s"
  short Club = "c"

instance Short Face where
  short Ace = "A"
  short Two = "2"
  short Three = "3"
  short Four = "4"
  short Five = "5"
  short Six = "6"
  short Seven = "7"
  short Eight = "8"
  short Nine = "9"
  short Ten = "T"
  short Jack = "J"
  short Queen = "Q"
  short King = "K"

instance Short Card where
  short (Card s f) = short f ++ short s

shuffle :: StdGen -> [a] -> [a]
shuffle gen xs = map fst $ sortBy (compare `on` snd) zipped
  where zipped = zip xs (randoms gen :: [Int])

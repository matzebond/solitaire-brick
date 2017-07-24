{-# LANGUAGE FlexibleContexts #-}
module Solitaire where

import Data.Array
import Data.Maybe
import qualified Data.Foldable as F (and)
import System.Random
import Control.Monad
import Control.Monad.State.Lazy
import PlayingCards
import MyUtil

data Column = Column { sequence :: [Card],
                       hiddenStack :: [Card]}
            deriving (Show)

data SolitaireGame = SolitaireGame { drawStack :: [Card],
                                     wastePile :: [Card],
                                     foundations :: Array Int [Card],
                                     tableau :: Array Int Column,
                                     gameSettings :: SolitaireGameSettings}

data SolitaireGameSettings = SolitaireGameSettings { drawThreeMode :: Bool,
                                                     playStackNum :: Int }

defaultSettings :: SolitaireGameSettings
defaultSettings = SolitaireGameSettings True 7

data SolitairePos = WastePile
                 | Foundation Int
                 | Tableau Int Int 

initGameFull :: SolitaireGameSettings -> IO SolitaireGame
initGameFull settings = do
  rng <- getStdGen
  let shuffledDeck = shuffle rng fullDeck
  return $ initGame settings shuffledDeck

-- build a SolitaireGame from a shuffled deck
initGame :: SolitaireGameSettings -> Deck -> SolitaireGame
initGame s deck = (SolitaireGame remainDeck [] found tab s)
  where n = playStackNum s
        hiddenCardsNum = n*(n+1) `div` 2
        (hiddenCards, remainDeck) = splitAt hiddenCardsNum deck
        hiddenStacks = map (\x -> Column [head x] (tail x)) $ splitStacks hiddenCards
        found = listArray (0,3) []
        tab = (listArray (0,(n-1)) hiddenStacks)

nonShuffledSolitaireGame :: SolitaireGame
nonShuffledSolitaireGame = initGame defaultSettings fullDeck

-- put wastePile on drawStack
redeal :: SolitaireGame -> SolitaireGame
redeal (SolitaireGame drawS waste found tab s) = (SolitaireGame (drawS ++ reverse waste) [] found tab s)


-- drawCards according to the rules from drawStack to the wastePile (redeal if necessary)
drawCards :: SolitaireGame -> SolitaireGame
drawCards game@(SolitaireGame drawS waste found tab s)
  | null drawS = drawCards $ redeal game
  | drawThreeMode s = let (top,rest) = splitAt 3 drawS in
                        (SolitaireGame rest (reverse top ++ waste) found tab s)
  | otherwise = SolitaireGame (init drawS) (head drawS : waste) found tab s


-- pick up a card/collection from a position
getCards :: SolitairePos -> SolitaireGame -> (Maybe [Card], SolitaireGame)
getCards WastePile game@(SolitaireGame (x:drawS) waste found tab s) =
  (Just [x], SolitaireGame drawS waste found tab s)
getCards (Foundation foundPos) game@(SolitaireGame drawS waste found tab s) =
  if null stack then (Nothing, game)
  else (Just [head stack], SolitaireGame drawS waste newFound tab s)
  where stack = found ! foundPos
        -- (card,newStack) = splitAt 1 stack
        newFound = found // [(foundPos, tail stack)]
getCards (Tableau tabPos tabIndex) game@(SolitaireGame drawS waste found tab s) =
  if null stack then (Nothing, game)
  else if length stack < tabIndex then (Nothing, game)
       else (Just top, SolitaireGame drawS waste found newTab s)
  where (Column stack hidden) = tab ! tabPos
        (top,newStack) = splitAt tabIndex stack
        newTab = tab // [(tabPos, Column newStack hidden)]
getCards _ game = (Nothing, game)


-- is a Card a successor of an other Card, by the rules of solitair
isSucc :: Card -> Card -> Bool
isSucc (Card s1 f1) (Card s2 f2) = if f1 == succ f2 && colorSuit s1 /= colorSuit s2 then True else False


validCardMove :: Card -> SolitairePos -> [Card] -> Bool
validCardMove (Card _ Ace) (Foundation _) [] = True
validCardMove (Card _ King)  (Tableau _ 0) [] = True
validCardMove c (Foundation _) (x:_) = c `isSucc` x
validCardMove c (Tableau _ 0) (x:_) =  x `isSucc` c
validCardMove _ _ _ = False


-- put down a card/collection on to a position
appendCards :: [Card] -> SolitairePos -> SolitaireGame -> (Bool, SolitaireGame)
appendCards [x] pos@(Foundation foundPos) game@(SolitaireGame drawS waste found tab s) =
  if not $ validCardMove x pos stack then (False, game)
  else (True, SolitaireGame drawS waste newFound tab s)
  where stack = found ! foundPos
        newFound = found // [(foundPos, x:stack)]
appendCards cards pos@(Tableau tabPos _) game@(SolitaireGame drawS waste found tab s) =
  if null cards then (False, game)
  else if not $ validCardMove (last cards) pos stack then (False, game)
       else (True, SolitaireGame drawS waste found newTab s)
  where (Column stack hidden) = tab ! tabPos
        newTab = tab // [(tabPos, Column (cards ++ stack) hidden)]
appendCards _ _ game = (False, game)

-- pick up cards from one position and try to put them down at another
moveCards :: SolitairePos -> SolitairePos -> SolitaireGame -> (Bool, SolitaireGame)
moveCards fromPos toPos game = runState combined game
  where combined = do cards <- state $ getCards fromPos
                      case cards of
                        Nothing -> return False
                        Just c -> state $ appendCards c toPos

columnEmpty :: Column -> Bool
columnEmpty (Column [] []) = True
columnEmpty _ = False

checkWin :: SolitaireGame -> Bool
checkWin game@(SolitaireGame drawS waste found tab _) =
  null drawS && null waste && F.and (fmap columnEmpty tab)

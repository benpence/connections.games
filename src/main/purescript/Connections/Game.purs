module Connections.Game
  ( assassins
  , blueRemaining
  , redRemaining
  , winner
  ) where

import Data.Array                                as Array

import Data.Maybe (Maybe(..))

import Connections.Types
import Prelude

-- | A winning team, if any
winner :: Game -> Maybe Turn
winner game@(Game { turn })
    | (redRemaining game)  == 0 || (assassinGuessed game && turn == RedTurn)  = Just RedTurn
    | (blueRemaining game) == 0 || (assassinGuessed game && turn == BlueTurn) = Just BlueTurn
    | otherwise                                                               = Nothing

-- | How many red squares haven't been guessed yet
redRemaining :: Game -> Int
redRemaining = countSquaresBy (unguessed Red)

-- | How many blue squares haven't been guessed yet
blueRemaining :: Game -> Int
blueRemaining = countSquaresBy (unguessed Blue)

-- | How many assassins are there on the board?
assassins :: Game -> Int
assassins = countSquaresBy (\(Square square) -> square.squareType == Assassin)

-- | Has an assassin been guessed?
assassinGuessed :: Game -> Boolean
assassinGuessed game =
  let
    isGuessed (Square square) = square.squareType == Assassin && square.guessed
  in
    (countSquaresBy isGuessed game) /= 0

unguessed :: SquareType -> Square -> Boolean
unguessed color (Square square) = square.squareType == color && not square.guessed

-- | Count the number of squares on the board that fulfill some predicate
countSquaresBy :: (Square -> Boolean) -> Game -> Int
countSquaresBy pred (Game { board: (Board board) }) =
  let
    countRow = Array.length <<< (Array.filter pred)
  in
    Array.foldl (+) 0 (map countRow board)

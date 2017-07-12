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

winner :: Game -> Maybe Turn
winner game@(Game { turn })
    | (redRemaining game)  == 0 || (assassinGuessed game && turn == RedTurn)  = Just RedTurn
    | (blueRemaining game) == 0 || (assassinGuessed game && turn == BlueTurn) = Just BlueTurn
    | otherwise                                                               = Nothing

redRemaining :: Game -> Int
redRemaining = countSquaresBy (unguessed Red)

blueRemaining :: Game -> Int
blueRemaining = countSquaresBy (unguessed Blue)

assassins :: Game -> Int
assassins = countSquaresBy (\(Square square) -> square.squareType == Assassin)

assassinGuessed :: Game -> Boolean
assassinGuessed game =
  let
    isGuessed (Square square) = square.squareType == Assassin && square.guessed
  in
    (countSquaresBy isGuessed game) /= 0

unguessed :: SquareType -> Square -> Boolean
unguessed color (Square square) = square.squareType == color && not square.guessed

countSquaresBy :: (Square -> Boolean) -> Game -> Int
countSquaresBy pred (Game { board: (Board board) }) =
  let
    countRow = Array.length <<< (Array.filter pred)
  in
    Array.foldl (+) 0 (map countRow board)

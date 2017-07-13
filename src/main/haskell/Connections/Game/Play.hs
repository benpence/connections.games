{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Connections.Game.Play where

import qualified Data.Array                      as Array

import Data.Text (Text)
import GHC.Generics (Generic)

import Data.Array ((//))
import Data.Monoid ((<>))
import Connections.Util (showT)

import Connections.Types

data Move
    = Guess (Int, Int)
    | EndTurn
    deriving (Eq, Show, Ord, Generic)

type InvalidMove = Text

makeMove :: Game -> Move -> Either InvalidMove Game
makeMove game@(Game {..}) guess@(Guess (i, j))
  | i < minI || maxI < i || j < minJ || maxJ < j =
      Left ("Guess (" <> showT i <> ", " <> showT j <> ") is out of bounds.")
  | squareGuessed =
      Left ("Square (" <> showT i <> ", " <> showT j <> ") already guessed.")
  | otherwise =
      Right (game
        { gameTurn = updatedTurn
        , gameBoard = Board updatedBoard
        }
      )
  where 
    -- Examine game
    (Board boardMatrix)          = gameBoard
    ((minI, minJ), (maxI, maxJ)) = Array.bounds boardMatrix

    square@(Square {..})         = boardMatrix Array.! (i, j)

    -- Transform game
    correctGuess                 = ((squareType == Red)  && (gameTurn == RedTurn)) ||
                                   ((squareType == Blue) && (gameTurn == BlueTurn))
    updatedTurn                  = if correctGuess then gameTurn else succ gameTurn

    updatedSquare                = square { squareGuessed = True }
    updatedBoard                 = boardMatrix // [((i, j), updatedSquare)]
makeMove game EndTurn = Right (game { gameTurn = succ (gameTurn game) })

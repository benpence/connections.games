{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Connections.Game.Create where

import qualified Connections.Util                as Util
import qualified Data.Aeson.Types                as Aeson
import qualified Data.Array                      as Array
import qualified Data.List                       as List
import qualified Data.Set                        as Set
import qualified System.Random                   as Random

import Data.Set (Set)
import Data.Text (Text)

import Connections.Types

newGame :: Board -> Game
newGame board = Game RedTurn board

data BoardConfig
    = BoardConfig
        { boardHeight :: Int
        , boardWidth  :: Int
        , redWords    :: Int
        , blueWords   :: Int
        , assassins   :: Int
        }

defaultBoardConfig :: BoardConfig
defaultBoardConfig = BoardConfig
  { boardHeight = 5
  , boardWidth  = 5
  , redWords    = 7
  , blueWords   = 6
  , assassins   = 1
  }

type InvalidConfig = Text

randomBoard :: (Random.RandomGen g) => BoardConfig -> [Text] -> g -> Either InvalidConfig (Board, g)
randomBoard (BoardConfig { .. }) dictionary randomGen
  | boardHeight < 1 = Left "Board Height < 1"
  | boardWidth < 1  = Left "Board Width < 1"
  | redWords < 1    = Left "Red words < 1"
  | blueWords < 1   = Left "Blue words < 1"
  | assassins < 1   = Left "Assassins < 1"
  | length dictionary < boardHeight * boardWidth = Left "Dictionary too small"
  | boardHeight * boardWidth < redWords + blueWords + assassins = Left "Board not big enough"
  | otherwise =
  let
    squaresCount       = boardHeight * boardWidth
    markedSquaresCount = redWords + blueWords + assassins
    (words, gen1)      = Util.chooseN randomGen squaresCount dictionary

    markedSquares = 
        List.replicate redWords Red ++
        List.replicate blueWords Blue ++
        List.replicate assassins Assassin
    greySquares        = List.replicate (squaresCount - markedSquaresCount) Grey
    (squares, gen2)    = Util.chooseN gen1 squaresCount (greySquares ++ markedSquares)

    boardArray = Array.listArray ((0, 0), (boardHeight - 1, boardWidth - 1))
        [ Square { squareGuessed = False
                 , squareType    = squareType
                 , squareWord    = word
                 }
        | (index, squareType, word) <- zip3 [0..] squares words
        ]
  in
    Right (Board boardArray, gen2)

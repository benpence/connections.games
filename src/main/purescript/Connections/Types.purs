module Connections.Types
  ( Board(..)
  , Game(..)
  , Square(..)
  , SquareType(..)
  , Turn(..)
  ) where

import Data.Array                                as Array
import Data.Tuple                                as Tuple

import Data.Argonaut.Decode (class DecodeJson, (.?), decodeJson)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))

import Prelude

newtype Game = Game
    { turn      :: Turn
    , board     :: Board
    }

instance decodeGame :: DecodeJson Game where
    decodeJson json = do
        obj   <- decodeJson json
        turn  <- obj .? "gameTurn"
        board <- obj .? "gameBoard"
        pure (Game { turn, board })

data Turn = RedTurn | BlueTurn

instance eqTurn :: Eq Turn where
    eq RedTurn  RedTurn  = true
    eq BlueTurn BlueTurn = true
    eq _ _               = false

instance decodeTurn :: DecodeJson Turn where
    decodeJson json = do
        str <- decodeJson json
        case str of
            "RedTurn"  -> Right RedTurn
            "BlueTurn" -> Right BlueTurn
            _          -> Left "Turn must be one of [\"RedTurn\", \"BlueTurn\"]"

-- | A 2D matrix of squares. A list of horizontal rows of squares.
newtype Board = Board (Array (Array Square))

instance decodeBoard :: DecodeJson Board where
    decodeJson json = do
        obj <- decodeJson json
        pure (jsonToBoard obj)

-- | Convert square key-values into nested arrays. This is just an adaptor from
-- the format that the backend serializes the 2D board as to our frontend format.
jsonToBoard :: Array (Tuple (Tuple Int Int) Square) -> Board
jsonToBoard squares =
  let
    sameRow (Tuple (Tuple a _) _) (Tuple (Tuple b _) _) = a == b
    groupedByRow   = Array.groupBy sameRow squares

    projectSquares = map Tuple.snd

    rowsOfSquares  = map (Array.fromFoldable <<< projectSquares) groupedByRow
  in
    Board rowsOfSquares

newtype Square = Square
    { guessed    :: Boolean
    , squareType :: SquareType
    , word       :: String
    }

instance decodeSquare :: DecodeJson Square where
    decodeJson json = do
        obj        <- decodeJson json
        guessed    <- obj .? "squareGuessed"
        squareType <- obj .? "squareType"
        word       <- obj .? "squareWord"
        pure (Square { guessed, squareType, word })

data SquareType
    = Red
    | Blue
    | Grey
    | Assassin

instance decodeSquareType :: DecodeJson SquareType where
    decodeJson json = do
        str <- decodeJson json
        case str of
            "Red"      -> Right Red
            "Blue"     -> Right Blue
            "Grey"     -> Right Grey
            "Assassin" -> Right Assassin
            _          -> Left "SquareType must be one of [\"Red\", \"Blue\", \"Grey\", \"Assassin\"]"

instance eqSquareType :: Eq SquareType where
    eq Red      Red      = true
    eq Blue     Blue     = true
    eq Grey     Grey     = true
    eq Assassin Assassin = true
    eq _ _               = false

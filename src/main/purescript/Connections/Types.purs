module Connections.Types
  ( Board(..)
  , Game(..)
  , Square(..)
  , SquareType(..)
  , Turn(..)
  ) where

import Data.Argonaut.Decode (class DecodeJson, (.?), decodeJson)
import Data.Either (Either(..))

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

instance decodeTurn :: DecodeJson Turn where
    decodeJson json = do
        str <- decodeJson json
        case str of
            "RedTurn"  -> Right RedTurn
            "BlueTurn" -> Right BlueTurn
            _          -> Left "Turn must be one of [\"RedTurn\", \"BlueTurn\"]"

newtype Board = Board (Array (Array Square))

instance decodeBoard :: DecodeJson Board where
    decodeJson json = do
        obj <- decodeJson json
        pure (Board obj)

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

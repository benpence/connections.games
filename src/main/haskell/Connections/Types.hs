{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Connections.Types where

import qualified Data.Aeson.Types                as Aeson
import qualified Data.Array                      as Array

import Data.Array (Array)
import Data.Text (Text)
import GHC.Generics (Generic)

data Game
    = Game
        { gameTurn      :: Turn
        , gameBoard     :: Board
        }
    deriving (Eq, Show, Ord, Generic)

instance Aeson.ToJSON Game

data Turn
    = RedTurn
    | BlueTurn
    deriving (Eq, Enum, Show, Ord, Generic)

instance Aeson.ToJSON Turn

newtype Board
    = Board (Array (Int, Int) Square)
    deriving (Eq, Show, Ord, Generic)

instance Aeson.ToJSON Board where
    toJSON (Board array) = Aeson.toJSON (Array.assocs array)

data Square
    = Square
        { squareGuessed :: Bool
        , squareType    :: SquareType
        , squareWord    :: Text
        }
    deriving (Eq, Show, Ord, Generic)

instance Aeson.ToJSON Square

data SquareType
    = Red
    | Blue
    | Grey
    | Assassin
    deriving (Eq, Show, Ord, Generic)

instance Aeson.ToJSON SquareType

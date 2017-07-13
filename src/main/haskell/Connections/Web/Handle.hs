{-# LANGUAGE OverloadedStrings #-}

module Connections.Web.Handle where

import qualified Connections.Game.Create         as Create
import qualified Connections.Game.Play           as Play
import qualified Connections.Web.Store           as Store
import qualified System.Random                   as Random

import Connections.Web.Store (Store)
import Connections.Util (showT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid ((<>))
import Data.Text (Text)

import Connections.Types    

newGame :: (MonadIO io)
        => Store io
        -> Store.Key
        -> Create.BoardConfig
        -> [Text]
        -> io (Either Create.InvalidConfig ())
newGame store key boardConfig dictionary = do
    stdGen <- liftIO Random.newStdGen

    let eitherBoard = Create.randomBoard boardConfig dictionary stdGen

    -- Persist new board
    case eitherBoard of
        Right (newBoard, _) -> do
            result <- (Store.setGame store) key (Create.newGame newBoard)
            pure (Right result)

        -- TODO: Log
        Left errorMessage -> do
            pure (Left errorMessage)

status :: (MonadIO io) => Store io -> Store.Key -> io (Maybe Game)
status store key = (Store.getGame store) key

move :: (MonadIO io) => Store io -> Store.Key -> Play.Move -> io (Either Play.InvalidMove ())
move store key move = do
    maybeGame <- (Store.getGame store) key

    let eitherUpdatedGame = case maybeGame of
            Just game -> Play.makeMove game move
            Nothing   -> Left ("Cannot make move " <> showT move <> " on non-existent game " <> showT key)

    -- TODO: Collapse this into previous expression
    case eitherUpdatedGame of
        Right updatedGame -> do
            (Store.setGame store) key updatedGame
            pure (Right ())

        -- TODO: Log
        Left errorMessage -> do
            pure (Left errorMessage)

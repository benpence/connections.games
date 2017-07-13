{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Connections.Web.Controller where

import qualified Connections.Game.Create         as Create
import qualified Connections.Game.Play           as Play
import qualified Connections.Web.Handle          as Handle
import qualified Connections.Web.Store           as Store
import qualified Data.Aeson.Types                as Aeson

import Connections.Web.Store (Store)
import Connections.Util (showT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.=))
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Generics (Generic)

data Action
    = NewGame
    | Status
    | Move Play.Move

data AppConfig
    = AppConfig
        { boardConfig :: Create.BoardConfig
        , dictionary  :: [Text]
        }

data ApiResponse
    = Results
        { results :: Aeson.Value
        }
    | Errors
        { errors :: [Text]
        }
    deriving (Eq, Show, Generic)

instance Aeson.ToJSON ApiResponse where
    toJSON (Results result) = Aeson.object ["results" .= result]
    toJSON (Errors errors)  = Aeson.object ["errors"  .= errors]

onAction :: (MonadIO io) => AppConfig -> Store io -> Store.Key -> Action -> io ApiResponse
onAction (AppConfig {..}) store key NewGame = do
    result <- Handle.newGame store key boardConfig dictionary

    let apiResponse = case result of
            Right ()          -> Results Aeson.Null
            Left errorMessage -> Errors [errorMessage]

    pure apiResponse

onAction _ store key Status = do
    maybeGame <- Handle.status store key

    let apiResponse = Results (Aeson.toJSON maybeGame)

    pure apiResponse

onAction (AppConfig {..}) store key (Move move) = do
    result <- Handle.move store key move

    let apiResponse = case result of
            Right ()          -> Results Aeson.Null
            Left errorMessage -> Errors [errorMessage]

    pure apiResponse

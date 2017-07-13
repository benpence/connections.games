{-# LANGUAGE OverloadedStrings #-}

module Connections.Web.Route
  ( apiRoutes
  , staticRoutes
  ) where

import qualified Connections.Game.Play           as Play
import qualified Connections.Web.Controller      as Controller
import qualified Connections.Web.Store           as Store
import qualified Data.Aeson.Types                as Aeson
import qualified Data.Text                       as Text
import qualified Paths_connections               as Cabal
import qualified System.IO                       as IO
import qualified Web.Scotty                      as Scotty

import Connections.Web.Store (Store)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson.Types
import Data.Monoid ((<>))
import Data.Text (Text)
import Text.Read (readMaybe)

-- | Add routes for routes the browser might visit directly.
staticRoutes :: Text -> Scotty.ScottyM ()
staticRoutes staticDirectory = do
    -- | Load the HTML page that will download and execute the thick JS client.
    Scotty.get "/" $ do
        Scotty.file "src/main/resources/static/index.html"

    -- | Serve static files from a directory
    serveStaticDirectory "/static/" staticDirectory

serveStaticDirectory :: Text -> Text -> Scotty.ScottyM ()
serveStaticDirectory staticRouteDirectory staticDirectory = do
    let routePath = Text.unpack ("^" <> staticRouteDirectory <> "(.*)$")

    Scotty.get (Scotty.regex routePath) $ do
        path <- Scotty.param "1"
        let relativePath = staticDirectory <> path
        absolutePath <- liftIO (Cabal.getDataFileName (Text.unpack relativePath))

        -- TODO: Is this vulnerable to directory traversal attacks? Doesn't seem
        -- like it from testing, but I should look into this more
        Scotty.file absolutePath

-- | Add routes for all the API calls
--
-- Each route returns JSON
--   On success: { results: ... }
--   On failure: { errors: ["..."] }
apiRoutes :: Controller.AppConfig -> Store IO -> Scotty.ScottyM ()
apiRoutes appConfig store = do
    let keyParam = fmap Store.Key (Scotty.param "key")

    -- | Ends the turn for the current team. Mutations will be visible in
    -- "/api/status". Returns `null` as a result.
    Scotty.get "/api/end_turn" $ do
        key <- keyParam

        let action = Controller.Move Play.EndTurn
        let controllerResponse = Controller.onAction appConfig store key action
        apiResponse <- liftIO controllerResponse
        Scotty.json apiResponse

    -- | Given the integer values for the parameters "i" and "j", guess the
    -- 0-indexed square (i, j). Mutations will be visible in "/api/status".
    -- Returns `null` as a result.
    Scotty.get "/api/guess" $ do
        key <- keyParam
        i   <- Scotty.param "i"
        j   <- Scotty.param "j"

        let action = Controller.Move (Play.Guess (i, j))
        let controllerResponse = Controller.onAction appConfig store key action
        apiResponse <- liftIO controllerResponse

        Scotty.json apiResponse

    -- | Generates a new game. Mutations will be visible in "/api/status".
    -- Returns `null` as a result.
    Scotty.get "/api/new_game" $ do
        key <- keyParam

        let controllerResponse = Controller.onAction appConfig store key Controller.NewGame
        apiResponse <- liftIO controllerResponse
        Scotty.json apiResponse

    -- | Reads the state of the game, if there is one; otherwise, return `null`
    -- as the result.
    Scotty.get "/api/status" $ do
        key <- keyParam

        let controllerResponse = Controller.onAction appConfig store key Controller.Status
        apiResponse <- liftIO controllerResponse
        Scotty.json apiResponse

{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Connections.Game.Create         as Create
import qualified Connections.Web.Controller      as Controller
import qualified Connections.Web.Route           as Route
import qualified Connections.Web.Store           as Store
import qualified Data.Text                       as Text
import qualified Paths_connections               as Cabal
import qualified System.Random                   as Random
import qualified Web.Scotty                      as Scotty

import Connections.Types
import Data.Monoid ((<>))
import Data.Text (Text)

resourcesDirectory :: Text
resourcesDirectory = "src/main/resources/"

main :: IO ()
main = do
    wordsPath <- Cabal.getDataFileName (Text.unpack (resourcesDirectory <> "words.txt"))
    wordList  <- fmap words (readFile wordsPath)

    store     <- Store.inMemory

    Scotty.scotty 3000 $ do
        --Scotty.get "/:word" $ do
        --    beam <- Scotty.param "word"
        --    Scotty.html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

        Route.staticRoutes (resourcesDirectory <> "static/")
    
        let appConfig = Controller.AppConfig
              { Controller.boardConfig = Create.BoardConfig
                  { Create.boardHeight = 5
                  , Create.boardWidth  = 5
                  , Create.redWords    = 7
                  , Create.blueWords   = 7
                  , Create.assassins   = 1
                  }
              , Controller.dictionary = map Text.pack wordList
              }
    
        Route.apiRoutes appConfig store

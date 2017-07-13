module Main where

import Connections.Api                           as Api
import Connections.View                          as View
import Pux                                       as Pux

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Network.HTTP.Affjax (AJAX)
import Signal.Channel (CHANNEL)
import Prelude

main :: forall r. Eff ("ajax" :: AJAX, "channel" :: CHANNEL, "err" :: EXCEPTION | r) Unit
main = do
    app <- Pux.start
        { initialState: View.init
        , update: View.update Api.remoteClient
        , view: View.view
        -- TODO: Add status check signal
        , inputs: [View.refreshEvery 2000] }

    Pux.renderToDOM "body" app.html

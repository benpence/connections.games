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
    routeChanges <- Route.changes

    app <- Pux.start
        { initialState: View.init
        , update: View.update Api.remoteClient
        , view: View.view
        , inputs: View.inputs routeChanges }

    Pux.renderToDOM "body" app.html

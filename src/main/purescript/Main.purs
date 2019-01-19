module Main where

import Connections.Api                           as Api
import Connections.View                          as View
import Pux                                       as Pux

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Network.HTTP.Affjax (AJAX)
import Signal.Channel (CHANNEL)
import Prelude

foreign import currentPath :: forall eff. Eff eff String

main :: forall r. Eff ("ajax" :: AJAX, "channel" :: CHANNEL, "err" :: EXCEPTION | r) Unit
main = do
    path <- currentPath
    app <- Pux.start
        { initialState: View.init path
        , update: View.update Api.remoteClient
        , view: View.view
        , inputs: [View.refreshEvery 2000] }

    Pux.renderToDOM "body" app.html

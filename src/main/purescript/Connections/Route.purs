module Connections.Route
  ( changes
  , navigateTo
  , Route(..)
  , match
  ) where

import Data.Maybe                                as Maybe
import Pux                                       as Pux
import Pux.Router                                as Router

import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))
import Control.Monad.Eff (Eff)
import Data.Functor ((<$))
import DOM (DOM)
import Signal (Signal)

import Prelude

data Route
    = Landing
    | GameKey String

changes :: forall eff. Eff (dom :: DOM | eff) (Signal Route)
changes = map (map match) Router.sampleUrl

navigateTo :: forall eff s. Route -> s -> Pux.EffModel s (dom :: DOM | eff)
navigateTo route s = Pux.onlyEffects s [Router.navigateTo (path route)]

path :: Route -> String
path Landing = "/"
path (GameKey gameKey) = "game/" <> gameKey

match :: String -> Route
match url = Maybe.fromMaybe Landing (Router.router url (gameKey <|> landing))

landing :: Router.Match Route
landing = Landing <$ Router.end

gameKey :: Router.Match Route
gameKey =
  let
    route = Router.lit "game" *> Router.str <* Router.end
  in
    map GameKey route

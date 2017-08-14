module Connections.View
  ( Event(..)
  , init
  , inputs
  , State(..)
  , update
  , view
  ) where

import Connections.Game                          as Game
import Connections.View.GameView                 as GameView
import Connections.View.LandingView              as LandingView
import Connections.Route                         as Route
import Pux                                       as Pux
import Pux.Router                                as Router

data State
    = LandingState IntroView.State
    | GameState GameView.State

data Event
    = LandingEvent IntroView.Event
    | GameEvent GameView.Event
    | RouteEvent Route.Route

init :: State
init = LandingState LandingView.init

inputs :: Signal Route.Route -> Array (Signal Event)
inputs routeChanges = [
  map GameEvent (GameView.refreshEvery 2000),
  map RouteEvent routeChanges
]

view :: State -> Html Event
view (LandingState state) = map LandingEvent (IntroView.view state)
view (GameState state)    = map GameEvent (GameView.view state)

update :: forall eff. Api.Client -> Event -> State -> Pux.EffModel State Event (ajax :: AJAX | eff)
update _      evt@(LandingEvent _) =
    mapPux LandingState LandingEvent (IntroView.update evt)

update client evt@(GameEvent _) =
    mapPux GameState    GameEvent    (GameView.update client evt)

update _          (RouteEvent Route.Landing) =
    Pux.noEffects (LandingState LandingView.init)
update _          (RouteEvent (Route.GameKey gameKey)) =
    Pux.noEffects (GameState (GameView.init gameKey))

-- | Wrap state and events from a smaller view into the main view
mapPux :: forall s e eff
        . (s -> State)                             -- ^ Wrap state
       -> (e -> Event)                             -- ^ Wrap HTML events
       -> (e -> s -> Pux.EffModel s e eff)         -- ^ The old update function
       -> (e -> s -> Pux.Effmodel State Event eff) -- ^ The wrapped update function
mapPux wrapState wrapEvents updateF =
    Pux.mapState wrapState (Pux.mapEffects wrapEvents (updateF))

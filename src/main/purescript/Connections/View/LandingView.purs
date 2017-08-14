module Connections.View.LandingView
  ( Event(..)
  , init
  , State(..)
  , update
  , view
  ) where

import Connections.Route                         as Route
import Pux                                       as Pux
import Pux.Html                                  as H
import Pux.Html.Attributes                       as A
import Pux.Html.Events                           as E
import Pux.Router                                as Router

import Pux.Html (Html)

import Prelude

type State = String

data Event
    = NewText String  -- ^ New text to display in input field  
    | JoinGame String -- ^ Create a new game with key

init :: State
init = ""

view :: State -> Html Event
view currentText =
    H.div [A.className "choose"] [
        H.input [A.className "choose-box", A.type_ "text", E.onInput (NewText <<< _.target.value)] [
            H.text currentText
        ],
        H.button [A.className "choose-button", E.onClick (const (JoinGame currentText))] [
            H.text "Join Game"
        ]
    ]

update :: forall eff. Event -> State -> Pux.EffModel State Event eff
update (NewText newText) _ = Pux.noEffects newText
update (JoinGame gameKey) _ = Router.navigateTo ("")--Route.gameKeyRoute gameKey)
forall eff. String -> Eff (dom :: DOM | eff) Unit

module Connections.View
  ( Action(..)
  , init
  , refreshEvery
  , State(..)
  , update
  , view
  ) where

import Connections.Api                           as Api
import Data.Array                                as Array
import Data.Int                                  as Int
import Data.Maybe                                as Maybe
import Pux                                       as Pux
import Pux.Html                                  as H
import Pux.Html.Attributes                       as A
import Pux.Html.Events                           as E
import Signal.Time                               as Time

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX)
import Pux.Html (Html)
import Signal (Signal)

import Connections.Types
import Prelude
import Debug.Trace

type State =
    { key         :: String
    , isSpymaster :: Boolean
    , game        :: Maybe Game
    }

data Action
    = EndTurn
    | Guess (Tuple Int Int)
    | NewGame
    | GetStatus
    | ToggleSpymaster
    | SideEffects
    | GotStatus (Maybe Game)

init :: State
init = { key: "", isSpymaster: false, game: Nothing }

update :: forall eff. Api.Client -> Action -> State -> Pux.EffModel State Action (ajax :: AJAX | eff)
update client EndTurn               state =
  let
    effect = map (handleMutableCall client state) (client.endTurn state.key)
  in
    Pux.onlyEffects state [effect]
update client (Guess coordinates)   state =
  let
    effect = map (handleMutableCall client state) (client.guess state.key coordinates)
  in
    Pux.onlyEffects state [effect]
update client NewGame               state =
  let
    effect = map (handleMutableCall client state) (client.newGame state.key)
  in
    Pux.onlyEffects state [effect]

update client GetStatus             state =
  let
    effect = do
        apiResponse <- client.status state.key

        case apiResponse of
            (Api.Results { results: maybeGame }) -> pure (GotStatus maybeGame)
            -- TODO: Log and dispaly to user
            (Api.Errors { errors: errors })      -> pure (trace (show errors) \_ -> SideEffects)
  in
    Pux.onlyEffects state [effect]

update client ToggleSpymaster       state =
    Pux.noEffects (state { isSpymaster = not state.isSpymaster })

update client SideEffects           state = Pux.noEffects state

update client (GotStatus maybeGame) state = Pux.noEffects (state { game = maybeGame })

handleMutableCall :: Api.Client -> State -> Api.Response Unit -> Action
handleMutableCall client state (Api.Results _) = SideEffects
-- TODO: Log and dispaly to user
handleMutableCall client state (Api.Errors _)  = SideEffects

view :: State -> Html Action
view state =
  let
    scoreView   = map (viewScore <<< (\(Game game) -> game.board)) state.game
    buttonsView = viewButtons state.isSpymaster (Maybe.isJust state.game)
    boardView   = map (viewBoard <<< (\(Game game) -> game.board)) state.game

    components  = Array.catMaybes [scoreView, Just buttonsView, boardView]
  in
    H.div [A.className "app container-fluid"] components

viewScore :: Board -> Html Action
viewScore (Board board) =
  let
    countSquare squareType (Square square)   = (square.squareType == squareType) && (not square.guessed)
    countSquares squareType squares          = Array.length (Array.filter (countSquare squareType) squares)
    countBoard squareType                    = Array.foldl (+) 0 (map (countSquares squareType) board)

    redRemaining                             = countBoard Red
    blueRemaining                            = countBoard Blue
    assassins                                = countBoard Assassin
  in
    H.div [A.className "score"] [
        H.div [A.className "score-red"] [
            H.text ("Red Remaining: " <> show redRemaining)
        ],
        H.div [A.className "score-blue"] [
            H.text ("Blue Remaining: " <> show blueRemaining)
        ],
        H.div [A.className "score-assassin"] [
            H.text ("Assassins: " <> show assassins)
        ]
    ]

viewButtons :: Boolean -> Boolean -> Html Action
viewButtons isSpymaster gameStarted =
  let
    endTurnButton =
        if gameStarted
        then [H.button [E.onClick (const EndTurn)] [H.text "End Turn"]]
        else []

    teamClass             = if isSpymaster then "active" else "disabled"
    spymasterClass        = if isSpymaster then "disabled" else "active"
    toggleSpymasterButton =
        H.div [] [
            H.button [E.onClick (const ToggleSpymaster), A.className teamClass] [
                 H.text "Player"
            ],
            H.button [E.onClick (const ToggleSpymaster), A.className spymasterClass] [
                 H.text "Spymaster"
            ]
        ]
  in
    H.div [A.className "buttons"] (
        endTurnButton <> [
        H.button [E.onClick (const NewGame)] [H.text "New Game"],
        toggleSpymasterButton
    ])

-- TODO: Plumb isSpymaster through to change visibility of guessed
viewBoard :: Board -> Html Action
viewBoard (Board board) =
  let
    squareRowViews = Array.mapWithIndex viewSquareRow board
  in
    H.div [A.className "board"] squareRowViews

viewSquareRow :: Int -> Array Square -> Html Action
viewSquareRow rowNum squares =
  let
    squareViews = Array.mapWithIndex (viewSquare rowNum) squares
  in
    H.div [A.className "square-row row"] squareViews

viewSquare :: Int -> Int -> Square -> Html Action
viewSquare rowNum colNum (Square square) =
  let
    guess = Guess (Tuple rowNum colNum)
  in
    H.div [A.className "square col-xs-1"] [
        H.span [E.onClick (const guess)] [
             H.text square.word
        ]
    ]

refreshEvery :: Int -> Signal Action
refreshEvery frequencyMillis =
    map (const GetStatus) (Time.every (Int.toNumber frequencyMillis))

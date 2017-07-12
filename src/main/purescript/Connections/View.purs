module Connections.View
  ( Action(..)
  , init
  , refreshEvery
  , State(..)
  , update
  , view
  ) where

import Connections.Api                           as Api
import Connections.Game                          as Game
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
            (Api.Errors { errors: errors })      -> pure SideEffects
  in
    Pux.onlyEffects state [effect]

update client ToggleSpymaster       state =
    Pux.noEffects (state { isSpymaster = not state.isSpymaster })

update client SideEffects           state = Pux.onlyEffects state [pure GetStatus]

update client (GotStatus maybeGame) state = Pux.noEffects (state { game = maybeGame })

handleMutableCall :: Api.Client -> State -> Api.Response Unit -> Action
handleMutableCall client state (Api.Results _) = SideEffects
-- TODO: Log and dispaly to user
handleMutableCall client state (Api.Errors _)  = SideEffects

view :: State -> Html Action
view state =
  let
    scoreView   = map viewScore state.game
    buttonsView = viewButtons state.isSpymaster (Maybe.isJust state.game)
    boardView   = map (viewGame state.isSpymaster) state.game

    components  = Array.catMaybes [scoreView, Just buttonsView, boardView]
  in
    H.div [A.className "app container-fluid"] components

-- TODO: Break this up into more top level functions
viewScore :: Game -> Html Action
viewScore game@(Game { turn }) =
  let
    winner = Game.winner game

    status :: Maybe Turn -> Turn -> String
    status (Just BlueTurn) _        = "Blue wins!"
    status (Just RedTurn)  _        = "Red wins!"
    status _               RedTurn  = "Red turn"
    status _               BlueTurn = "Blue turn"
  in
    H.div [A.className "score"] [
        H.div [A.className "score-status"] [
            H.text (status winner turn)
        ],
        H.div [A.className "score-red"] [
            H.text ("Red Remaining: " <> show (Game.redRemaining game))
        ],
        H.div [A.className "score-blue"] [
            H.text ("Blue Remaining: " <> show (Game.blueRemaining game))
        ],
        H.div [A.className "score-assassin"] [
            H.text ("Assassins: " <> show (Game.assassins game))
        ]
    ]

viewButtons :: Boolean -> Boolean -> Html Action
viewButtons isSpymaster gameStarted =
  let
    endTurnButton =
        if gameStarted
        then [H.button [E.onClick (const EndTurn)] [H.text "End Turn"]]
        else []

    toggleText =
        if isSpymaster
        then "Change to Player"
        else "Change to Spymaster"
    toggleSpymasterButton =
        H.div [] [
            H.button [E.onClick (const ToggleSpymaster)] [
                 H.text toggleText
            ]
        ]
  in
    H.div [A.className "buttons"] (
        endTurnButton <> [
        H.button [E.onClick (const NewGame)] [H.text "New Game"],
        toggleSpymasterButton
    ])

viewGame :: Boolean -> Game -> Html Action
viewGame isSpymaster game@(Game { board: (Board board) }) =
  let
    isGameOver       = Maybe.isJust (Game.winner game)
    squareRowViews = Array.mapWithIndex (viewSquareRow isSpymaster isGameOver) board
  in
    H.div [A.className "board"] squareRowViews

viewSquareRow :: Boolean -> Boolean -> Int -> Array Square -> Html Action
viewSquareRow isSpymaster isGameOver rowNum squares =
  let
    squareViews = Array.mapWithIndex (viewSquare isSpymaster isGameOver rowNum) squares
  in
    H.div [A.className "square-row row"] squareViews

viewSquare :: Boolean -> Boolean -> Int -> Int -> Square -> Html Action
viewSquare isSpymaster isGameOver rowNum colNum (Square square) =
  let
    guess   = Guess (Tuple rowNum colNum)

    onClick =
        -- Spymaster cannot click squares
        if isGameOver || square.guessed || isSpymaster
        then []
        else [E.onClick (const guess)]

    colorClassName :: SquareType -> String
    colorClassName _ | not isSpymaster && not square.guessed = "square-blank"
    colorClassName Red                                       = "square-red"
    colorClassName Blue                                      = "square-blue"
    colorClassName Grey                                      = "square-grey"
    colorClassName Assassin                                  = "square-assassin"

    className = "col-xs-1 square " <> colorClassName square.squareType
  in
    H.div ([A.className className] <> onClick) [
        H.text square.word
    ]

refreshEvery :: Int -> Signal Action
refreshEvery frequencyMillis =
    map (const GetStatus) (Time.every (Int.toNumber frequencyMillis))

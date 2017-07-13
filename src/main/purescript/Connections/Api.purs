module Connections.Api
  ( Client
  , dummyClient
  , remoteClient
  , Response(..)
  ) where

import Connections.Http                          as Http
import Data.Array                                as Array
import Data.HTTP.Method                          as Method
import Data.Path.Pathy                           as P

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff)
import Data.Argonaut.Core (Json, JObject)
import Data.Argonaut.Decode (class DecodeJson, (.?), decodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Path.Pathy ((</>))
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX)

import Connections.Types
import Prelude

data Response a
    = Results { results :: a }
    | Errors { errors :: (Array String) }

type Client =
    { endTurn :: forall r. String -> Aff (ajax :: AJAX | r) (Response Unit)
    , guess   :: forall r. String -> (Tuple Int Int) -> Aff (ajax :: AJAX | r) (Response Unit)
    , newGame :: forall r. String -> Aff (ajax :: AJAX | r) (Response Unit)
    , status  :: forall r. String -> Aff (ajax :: AJAX | r) (Response (Maybe Game))
    }

dummyClient :: Client
dummyClient =
  let
    square word = Square { guessed: false, squareType: Red, word: word }
    squareRow   = map (square <<< show) (Array.range 1 5)
    board       = map (const squareRow) (Array.range 1 5)
    game        = Game { turn: RedTurn, board: Board board }

    success     = Results { results: unit }
  in
    { endTurn: (\key -> pure success)
    , guess:   (\key squareCoordinates -> pure success)
    , newGame: (\key -> pure success)
    , status:  (\key -> pure (Results { results: Just game }))
    }

remoteClient :: Client
remoteClient =
    { endTurn: remoteEndTurn
    , guess:   remoteGuess
    , newGame: remoteNewGame
    , status:  remoteStatus
    }
    
remoteEndTurn :: forall r. String -> Aff (ajax :: AJAX | r) (Response Unit)
remoteEndTurn key =
  let
    path = P.rootDir </> P.dir "api" </> P.file "end_turn"
    url  = Http.path (Right path) [
        (Tuple "key" key)
    ]
  in do
    resp <- Http.request Method.GET url Nothing
    pure (toResponse (resp.response))

remoteGuess :: forall r. String -> (Tuple Int Int) -> Aff (ajax :: AJAX | r) (Response Unit)
remoteGuess key (Tuple i j) =
  let
    path = P.rootDir </> P.dir "api" </> P.file "guess"
    url  = Http.path (Right path) [
        (Tuple "key" key),
        (Tuple "i" (show i)),
        (Tuple "j" (show j))
    ]
  in do
    resp <- Http.request Method.GET url Nothing
    pure (toResponse (resp.response))

remoteNewGame :: forall r. String -> Aff (ajax :: AJAX | r) (Response Unit)
remoteNewGame key =
  let
    path = P.rootDir </> P.dir "api" </> P.file "new_game"
    url  = Http.path (Right path) [
        (Tuple "key" key)
    ]
  in do
    resp <- Http.request Method.GET url Nothing
    pure (toResponse (resp.response))

remoteStatus :: forall r. String -> Aff (ajax :: AJAX | r) (Response (Maybe Game))
remoteStatus key =
  let
    path = P.rootDir </> P.dir "api" </> P.file "status"
    url  = Http.path (Right path) [
        (Tuple "key" key)
    ]
  in do
    resp <- Http.request Method.GET url Nothing
    pure (toResponse (resp.response))

toResponse :: forall a. DecodeJson a => Json -> Response a
toResponse json = case decodeJson json of
    Right response    -> response
    Left errorMessage -> Errors { errors: [errorMessage] }

instance decodeResponse :: (DecodeJson a) => DecodeJson (Response a) where
    decodeJson json = do
        obj <- decodeJson json
        (decodeResults obj) <|> (decodeErrors obj)

decodeResults :: forall a. (DecodeJson a) => JObject -> Either String (Response a)
decodeResults obj = do
    results <- obj .? "results"
    pure (Results { results: results })

decodeErrors :: forall a. JObject -> Either String (Response a)
decodeErrors obj = do
    errors <- obj .? "errors"
    pure (Errors { errors: errors })

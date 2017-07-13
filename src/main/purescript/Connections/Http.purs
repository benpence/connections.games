module Connections.Http
  ( path
  , request
  ) where

import Data.List                                 as List
import Data.URI                                  as URI    
import Data.URI.Types                            as URI
import Network.HTTP.Affjax                       as Affjax

import Control.Monad.Aff (Aff)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.HTTP.Method (Method)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax.Response (class Respondable)

import Prelude

path :: URI.URIPathAbs -> Array (Tuple String String) -> String
path uriPath params =
  let
    paramsArray = map (\(Tuple param val) -> Tuple param (Just val)) (List.fromFoldable params)
    query = Just (URI.Query paramsArray)

    hierarchicalPart = URI.HierarchicalPart Nothing (Just uriPath)

    -- URI (Maybe URIScheme) HierarchicalPart (Maybe Query) (Maybe Fragment)
    uri = URI.URI Nothing hierarchicalPart query Nothing
  in
    URI.printURI uri

-- TODO: Ease constraint on payload to Network.HTTP.Affjax.Request.Requestable?
request :: forall a r. Respondable a => Method -> String -> Maybe String -> Aff (ajax :: AJAX | r) (Affjax.AffjaxResponse a)
request method url payload =
    Affjax.affjax (Affjax.defaultRequest { url = url, method = Left method, content = payload })

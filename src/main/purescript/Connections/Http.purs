module Connections.Http
  ( path
  , request
  ) where

import Data.List                                 as List
import Data.URI                                  as URI    
import Network.HTTP.Affjax                       as Affjax

import Control.Monad.Aff (Aff)
import Data.Either (Either(..))
import Data.HTTP.Method (Method)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax.Response (class Respondable)

import Prelude

-- | Construct path for URI with query params
path :: URI.URIPathAbs              -- ^ The absolute path of the URI
     -> Array (Tuple String String) -- ^ A list of query params as key-value pairs
     -> String
path uriPath params =
  let
    paramsArray = map (\(Tuple param val) -> Tuple param (Just val)) (List.fromFoldable params)
    query = Just (URI.Query paramsArray)

    hierarchicalPart = URI.HierarchicalPart Nothing (Just uriPath)

    -- URI (Maybe URIScheme) HierarchicalPart (Maybe Query) (Maybe Fragment)
    uri = URI.URI Nothing hierarchicalPart query Nothing
  in
    URI.printURI uri

request :: forall a r. Respondable a => Method -> String -> Maybe String -> Aff (ajax :: AJAX | r) (Affjax.AffjaxResponse a)
request method url payload =
    Affjax.affjax (Affjax.defaultRequest { url = url, method = Left method, content = payload })

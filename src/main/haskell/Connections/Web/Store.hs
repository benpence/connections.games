{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Connections.Web.Store where

import qualified Data.Map.Lazy                   as Map
import qualified GHC.Conc.Sync                   as Sync

import Control.Monad (Monad, (<=<))
import Data.Map.Lazy (Map)
import Data.Text (Text)

import Connections.Types

data Store m
    = Store
      { getGame :: Key -> m (Maybe Game)
      , setGame :: Key -> Game -> m ()
      }

newtype Key = Key Text deriving (Eq, Show, Ord)

type InMemoryStore = Sync.TVar (Map Key Game)

inMemory :: IO (Store IO)
inMemory = Sync.atomically $ do
    tStore <- Sync.newTVar Map.empty
    pure (inMemoryInit tStore)

inMemoryInit :: Sync.TVar (Map Key Game) -> Store IO
inMemoryInit tStore = Store
    -- TODO: Yuck! Refactor these
    { getGame = (\k -> Sync.atomically $ do
        inMemoryGetGame tStore k)
    , setGame = (\k g -> Sync.atomically $ do
        inMemorySetGame tStore k g)
    }


inMemoryGetGame :: InMemoryStore -> Key -> Sync.STM (Maybe Game)
inMemoryGetGame tStore key = do
    store <- Sync.readTVar tStore
    pure (Map.lookup key store)

inMemorySetGame :: InMemoryStore -> Key -> Game -> Sync.STM ()
inMemorySetGame tStore key game = do
    store <- Sync.readTVar tStore

    let updatedStore = Map.insert key game store
    Sync.writeTVar tStore updatedStore

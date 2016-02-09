{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Rumpus.Types where

import Data.Vault.Strict (Vault)
import Data.Map (Map)
import GHC.Word

import Control.Lens
import Control.Monad.State

import Data.Yaml

type EntityID = Word32

type EntityMap a = Map EntityID a

type ComponentName = String

type WorldMonad a = StateT World IO a

data ComponentInterface = ComponentInterface
    { ciAddComponent     :: (EntityID -> WorldMonad ())
    , ciRemoveComponent  :: (EntityID -> WorldMonad ())
    , ciExtractComponent :: Maybe (EntityID -> WorldMonad (Maybe Value))
    }

data World = World
    { _wldSystems          :: Vault
    , _wldComponents       :: Vault
    , _wldComponentLibrary :: Map ComponentName ComponentInterface
    , _wldEntities         :: [EntityID]
    }
makeLenses ''World

newWorld :: World
newWorld = World mempty mempty mempty mempty

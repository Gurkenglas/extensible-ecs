{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

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


data ECS = ECS
    { _wldSystems          :: Vault
    , _wldComponents       :: Vault
    , _wldComponentLibrary :: Map ComponentName ComponentInterface
    , _wldEntities         :: [EntityID]
    }



newECS :: ECS
newECS = ECS mempty mempty mempty mempty

data ComponentInterface = ComponentInterface
    { ciAddComponent     :: forall s m. (HasECS s, MonadState s m, MonadIO m) => (EntityID -> m ())
    , ciRemoveComponent  :: forall s m. (HasECS s, MonadState s m, MonadIO m) => (EntityID -> m ())
    , ciExtractComponent :: forall s m. (HasECS s, MonadState s m, MonadIO m) => Maybe (EntityID -> m (Maybe Value))
    }

class HasECS s where
    ecs :: Lens' s ECS

instance HasECS ECS where
    ecs = id

-- We can't use makeClassy to define the HasECS class and lenses, 
-- since the ComponentInterface and ECS types have a circular dependency,
-- meaning either ComponentInterface can't see the generated "HasECS" class
-- or ECS can't see the ComponentInterface type.
-- This just means we have to prefix our lenses with "ecs" internally, but that's not so bad.
makeLenses ''ECS

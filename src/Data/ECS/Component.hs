{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.ECS.Component where
import qualified Data.Vault.Strict as Vault
import Data.Vault.Strict (Key)
import Control.Lens
import Prelude hiding (lookup)
import Control.Monad.State
import qualified Data.Map as Map
import Data.Yaml hiding ((.=))

import Data.ECS.Types

registerComponent :: (HasECS s, MonadState s m) => String -> Key (EntityMap a) -> ComponentInterface -> m ()
registerComponent name componentKey componentInterface = do
    ecs . wldComponents %= Vault.insert componentKey mempty
    ecs . wldComponentLibrary . at name ?= componentInterface

registerComponentSimple :: (ToJSON a, MonadState s m, HasECS s) => String -> Key (EntityMap a) -> a -> m ()
registerComponentSimple name componentKey initialValue = 
    registerComponent name componentKey $ ComponentInterface 
        { ciAddComponent     = addComponent componentKey initialValue
        , ciRemoveComponent  = removeComponent componentKey
        , ciExtractComponent = Just (getComponentJSON componentKey)
        }

withComponentMap_ :: (HasECS s, MonadState s m) => Key (EntityMap a) -> ((EntityMap a) -> m b) -> m ()
withComponentMap_ componentKey = void . withComponentMap componentKey

withComponentMap :: (HasECS s, MonadState s m) => Key (EntityMap a) -> ((EntityMap a) -> m b) -> m (Maybe b)
withComponentMap componentKey action = do
    componentMaps <- use (ecs . wldComponents)
    forM (Vault.lookup componentKey componentMaps) action

-- | Perform an action on each entityID/component pair
traverseEntitiesWithComponent :: (HasECS s, MonadState s m) => Key (EntityMap a) -> ((EntityID, a) -> m b) -> m ()
traverseEntitiesWithComponent componentKey action = 
    withComponentMap_ componentKey $ \componentMap -> 
        forM_ (Map.toList componentMap) action

addComponent :: (HasECS s, MonadState s m) => Key (EntityMap a) -> a -> EntityID -> m ()
addComponent componentKey value entityID = 
    ecs . wldComponents %= Vault.adjust (Map.insert entityID value) componentKey

removeComponent :: (HasECS s, MonadState s m) => Key (EntityMap a) -> EntityID -> m ()
removeComponent componentKey entityID = 
    ecs . wldComponents %= Vault.adjust (Map.delete entityID) componentKey

withComponent :: (HasECS s, MonadState s m) => EntityID -> Key (EntityMap a) -> (a -> m b) -> m ()
withComponent entityID componentKey action = do
    maybeComponent <- getComponent entityID componentKey
    forM_ maybeComponent action

getComponent :: (HasECS s, MonadState s m) => EntityID -> Key (EntityMap a) -> m (Maybe a)
getComponent entityID componentKey = 
    fmap join $ withComponentMap componentKey $ \componentMap ->
        return $ Map.lookup entityID componentMap

getComponentJSON :: (HasECS s, MonadState s m, ToJSON a) => Key (EntityMap a) -> EntityID -> m (Maybe Value)
getComponentJSON componentKey entityID = fmap toJSON <$> getComponent entityID componentKey

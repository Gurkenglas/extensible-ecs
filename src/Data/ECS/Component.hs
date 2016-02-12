{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Data.ECS.Component where
import qualified Data.Vault.Strict as Vault
import Data.Vault.Strict (Key)
import Control.Lens
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Yaml hiding ((.=))
import Data.Maybe
import Data.ECS.Types



(==>) :: (MonadState s m, MonadReader EntityID m, HasComponents s) => Key (EntityMap a) -> a -> m ()
componentKey ==> value = addComponent componentKey value =<< ask

setComponentMap :: (MonadState s m, HasComponents s) => Key a -> a -> m ()
setComponentMap componentKey value = 
    components . unComponents %= Vault.insert componentKey value

lookupComponentMap :: (MonadState s m, HasComponents s) => Key a -> m (Maybe a)
lookupComponentMap componentKey = do
    Vault.lookup componentKey <$> use (components . unComponents)

modifyComponents :: (MonadState s m, HasComponents s) => Key a -> (a -> a) -> m ()
modifyComponents componentKey action = 
    components . unComponents %= Vault.adjust action componentKey

registerComponent :: MonadState ECS m => String -> Key (EntityMap a) -> ComponentInterface -> m ()
registerComponent name componentKey componentInterface = do
    setComponentMap componentKey mempty
    wldComponentLibrary . at name ?= componentInterface

registerComponentSimple :: (MonadState ECS m, ToJSON a) => String -> Key (EntityMap a) -> a -> m ()
registerComponentSimple name componentKey initialValue = 
    registerComponent name componentKey $ ComponentInterface 
        { ciAddComponent     = addComponent componentKey initialValue
        , ciRemoveComponent  = removeComponent componentKey
        , ciExtractComponent = Just (getComponentJSON componentKey)
        }

withComponentMap_ :: MonadState ECS m => Key (EntityMap a) -> ((EntityMap a) -> m b) -> m ()
withComponentMap_ componentKey = void . withComponentMap componentKey

withComponentMap :: (HasComponents s, MonadState s m) => Key (EntityMap a) -> ((EntityMap a) -> m b) -> m (Maybe b)
withComponentMap componentKey action = do
    componentMaps <- use (components . unComponents)
    forM (Vault.lookup componentKey componentMaps) action

getComponentMap :: (HasComponents s, MonadState s m) => Key r -> m r
getComponentMap componentKey = fromMaybe getComponentMapError <$> lookupComponentMap componentKey
    where getComponentMapError = error "getComponentMap couldn't find a componentMap for the given key"

-- | Perform an action on each entityID/component pair
forEntitiesWithComponent :: MonadState ECS m => Key (EntityMap a) -> ((EntityID, a) -> m b) -> m ()
forEntitiesWithComponent componentKey action = 
    withComponentMap_ componentKey $ \componentMap -> 
        forM_ (Map.toList componentMap) action



addComponent :: (HasComponents s, MonadState s m) => Key (EntityMap a) -> a -> EntityID -> m ()
addComponent componentKey value entityID = modifyComponents componentKey (Map.insert entityID value)

setComponent :: (HasComponents s, MonadState s m) => Key (EntityMap a) -> a -> EntityID -> m ()
setComponent = addComponent

removeComponent :: (HasComponents s, MonadState s m) => Key (EntityMap a) -> EntityID -> m ()
removeComponent componentKey entityID = modifyComponents componentKey (Map.delete entityID)

withComponent :: (HasComponents s, MonadState s m) => EntityID -> Key (EntityMap a) -> (a -> m b) -> m ()
withComponent entityID componentKey action = do
    maybeComponent <- getComponent entityID componentKey
    forM_ maybeComponent action

modifyComponent :: (HasComponents s, MonadState s m) => EntityID -> Key (EntityMap a) -> (a -> m a) -> m ()
modifyComponent entityID componentKey action = do
    maybeComponent <- getComponent entityID componentKey
    mapM action maybeComponent >>= \case
        Just newValue -> addComponent componentKey newValue entityID
        Nothing -> return ()

getComponent :: (HasComponents s, MonadState s m) => EntityID -> Key (EntityMap a) -> m (Maybe a)
getComponent entityID componentKey = 
    fmap join $ withComponentMap componentKey $ \componentMap ->
        return $ Map.lookup entityID componentMap

getComponentJSON :: (HasComponents s, MonadState s m, ToJSON a) => Key (EntityMap a) -> EntityID -> m (Maybe Value)
getComponentJSON componentKey entityID = fmap toJSON <$> getComponent entityID componentKey

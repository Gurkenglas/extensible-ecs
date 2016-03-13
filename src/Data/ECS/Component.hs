{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module Data.ECS.Component where
import qualified Data.ECS.Vault as Vault
import Data.ECS.Vault (Key)
import Control.Lens.Extra
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.HashMap.Strict as Map
import Data.Yaml hiding ((.=))
import Data.Maybe
import Data.ECS.Types

infixl 0 ==>
(==>) :: (MonadState s m, MonadReader EntityID m, HasComponents s) => Key (EntityMap a) -> a -> m ()
componentKey ==> value = setComponent componentKey value

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

newComponentInterface :: Key (EntityMap a) -> ComponentInterface
newComponentInterface componentKey = ComponentInterface 
    { ciAddComponent     = Nothing
    , ciRemoveComponent  = removeComponent componentKey
    , ciExtractComponent = Nothing
    , ciRestoreComponent = Nothing
    , ciDeriveComponent  = Nothing
    }

savedComponentInterface :: (ToJSON a, FromJSON a) => Key (EntityMap a) -> ComponentInterface
savedComponentInterface componentKey = (newComponentInterface componentKey)
    { ciExtractComponent = Just (getComponentJSON componentKey)
    , ciRestoreComponent = Just (setComponentJSON componentKey)
    }

-- | As in, this component should be added to new entites by default
defaultComponentInterface :: (ToJSON a, FromJSON a) => Key (EntityMap a) -> a -> ComponentInterface
defaultComponentInterface componentKey defaultValue = (savedComponentInterface componentKey)
    { ciAddComponent = Just (addComponent componentKey defaultValue) 
    }

withComponentMap_ :: (HasComponents s, MonadState s m) => Key (EntityMap a) -> ((EntityMap a) -> m b) -> m ()
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


setComponent :: (MonadReader EntityID m, HasComponents s, MonadState s m) => Key (EntityMap a) -> a -> m ()
setComponent componentKey value = setEntityComponent componentKey value =<< ask

addComponent :: (MonadReader EntityID m, HasComponents s, MonadState s m) => Key (EntityMap a) -> a -> m ()
addComponent = setComponent

setEntityComponent :: (HasComponents s, MonadState s m) => Key (EntityMap a) -> a -> EntityID -> m ()
setEntityComponent componentKey !value entityID = modifyComponents componentKey (Map.insert entityID value)

addEntityComponent :: (HasComponents s, MonadState s m) => Key (EntityMap a) -> a -> EntityID -> m ()
addEntityComponent = setEntityComponent

removeComponent :: (MonadReader EntityID m, HasComponents s, MonadState s m) => Key (EntityMap a) -> m ()
removeComponent componentKey = removeEntityComponent componentKey =<< ask

removeEntityComponent :: (HasComponents s, MonadState s m) => Key (EntityMap a) -> EntityID -> m ()
removeEntityComponent componentKey entityID = modifyComponents componentKey (Map.delete entityID)

withComponent :: (MonadReader EntityID m, HasComponents s, MonadState s m) => Key (EntityMap a) -> (a -> m b) -> m (Maybe b)
withComponent componentKey action = do
    entityID <- ask
    withEntityComponent entityID componentKey action

withComponent_ :: (MonadReader EntityID m, HasComponents s, MonadState s m) => Key (EntityMap a) -> (a -> m b) -> m ()
withComponent_ componentKey = void . withComponent componentKey

withEntityComponent :: (MonadState s m, HasComponents s) => EntityID -> Key (EntityMap a) -> (a -> m b) -> m (Maybe b)
withEntityComponent entityID componentKey action = mapM action =<< getEntityComponent entityID componentKey

withEntityComponent_ :: (MonadState s m, HasComponents s) => EntityID -> Key (EntityMap a) -> (a -> m b) -> m ()
withEntityComponent_ entityID componentKey = void . withEntityComponent entityID componentKey

modifyComponent :: (MonadReader EntityID m, HasComponents s, MonadState s m) => Key (EntityMap a) -> (a -> m a) -> m ()
modifyComponent componentKey action = ask >>= \eid -> modifyEntityComponent eid componentKey action

modifyEntityComponent :: (HasComponents s, MonadState s m) => EntityID -> Key (EntityMap a) -> (a -> m a) -> m ()
modifyEntityComponent entityID componentKey action = do
    maybeComponent <- getEntityComponent entityID componentKey
    mapM action maybeComponent >>= \case
        Just newValue -> setEntityComponent componentKey newValue entityID
        Nothing -> return ()

getComponent :: (HasComponents s, MonadState s m, MonadReader EntityID m) => Key (EntityMap a) -> m (Maybe a)
getComponent componentKey = ask >>= \eid -> getEntityComponent eid componentKey

getEntityComponent :: (HasComponents s, MonadState s m) => EntityID -> Key (EntityMap a) -> m (Maybe a)
getEntityComponent entityID componentKey = 
    fmap join $ withComponentMap componentKey $ \componentMap ->
        return $ Map.lookup entityID componentMap

entityHasComponent :: (MonadState s m, HasComponents s) => EntityID -> Key (EntityMap a) -> m Bool
entityHasComponent entityID componentKey = isJust <$> getEntityComponent entityID componentKey

getComponentJSON :: (MonadReader EntityID m, HasComponents s, MonadState s m, ToJSON a) => Key (EntityMap a) -> m (Maybe Value)
getComponentJSON componentKey = fmap toJSON <$> getComponent componentKey

setComponentJSON :: (MonadReader EntityID m, MonadIO m, MonadState s m, FromJSON a, HasComponents s) => Key (EntityMap a) -> Value -> m ()
setComponentJSON componentKey jsonValue = case flip parseEither jsonValue parseJSON of
    Right value -> setComponent componentKey value
    Left anError -> liftIO $ putStrLn ("setComponentJSON error: " ++ show anError)



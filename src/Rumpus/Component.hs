{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Rumpus.Component where
import qualified Data.Vault.Strict as Vault
import Data.Vault.Strict (Key)
import Control.Lens
import Prelude hiding (lookup)
import Control.Monad.State
import qualified Data.Map as Map
import Data.Yaml hiding ((.=))

import Rumpus.Types

registerComponent :: MonadState World m => String -> Key (EntityMap a) -> ComponentInterface -> m ()
registerComponent name componentKey componentInterface = do
    wldComponents %= Vault.insert componentKey mempty
    wldComponentLibrary . at name ?= componentInterface

registerComponentSimple :: (MonadState World m, ToJSON a) => String -> Key (EntityMap a) -> a -> m ()
registerComponentSimple name componentKey initialValue = 
    registerComponent name componentKey $ ComponentInterface 
        { ciAddComponent     = addComponentToEntity componentKey initialValue
        , ciRemoveComponent  = removeComponentFromEntity componentKey
        , ciExtractComponent = Just (getComponentJSON componentKey)
        }

withComponentMap_ :: MonadState World m => Key (EntityMap a) -> ((EntityMap a) -> m b) -> m ()
withComponentMap_ componentKey = void . withComponentMap componentKey

withComponentMap :: MonadState World m => Key (EntityMap a) -> ((EntityMap a) -> m b) -> m (Maybe b)
withComponentMap componentKey action = do
    componentMaps <- use wldComponents
    forM (Vault.lookup componentKey componentMaps) action

-- | Perform an action on each entityID/component pair
traverseEntitiesWithComponent :: MonadState World m => Key (EntityMap a) -> ((EntityID, a) -> m b) -> m ()
traverseEntitiesWithComponent componentKey action = 
    withComponentMap_ componentKey $ \componentMap -> 
        forM_ (Map.toList componentMap) action

addComponentToEntity :: (MonadIO m, MonadState World m) => Key (EntityMap a) -> a -> EntityID -> m ()
addComponentToEntity componentKey value entityID = 
    wldComponents %= Vault.adjust (Map.insert entityID value) componentKey

removeComponentFromEntity :: (MonadState World m) => Key (EntityMap a) -> EntityID -> m ()
removeComponentFromEntity componentKey entityID = 
    wldComponents %= Vault.adjust (Map.delete entityID) componentKey

withComponent :: MonadState World m => EntityID -> Key (EntityMap a) -> (a -> m b) -> m ()
withComponent entityID componentKey action = do
    maybeComponent <- getComponent entityID componentKey
    forM_ maybeComponent action

getComponent :: MonadState World m => EntityID -> Key (EntityMap a) -> m (Maybe a)
getComponent entityID componentKey = 
    fmap join $ withComponentMap componentKey $ \componentMap ->
        return $ Map.lookup entityID componentMap

getComponentJSON :: (MonadState World m, ToJSON a) => Key (EntityMap a) -> EntityID -> m (Maybe Value)
getComponentJSON componentKey entityID = fmap toJSON <$> getComponent entityID componentKey

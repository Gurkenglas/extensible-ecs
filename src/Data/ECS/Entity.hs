{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.ECS.Entity where
import Control.Lens
import Prelude hiding (lookup)
import Control.Monad.State
import qualified Data.Map as Map
import System.Random
import Data.List
import Data.ECS.Types


newEntity :: MonadIO m => m EntityID
newEntity = liftIO randomIO


createEntity :: (MonadIO m, HasECS s, MonadState s m) => m EntityID
createEntity = do
    entityID <- newEntity
    library <- use (ecs . wldComponentLibrary)
    forM_ library (\ComponentInterface{..} -> ciAddComponent entityID)
    ecs . wldEntities %= (entityID:)

    return entityID


removeEntity :: (HasECS s, MonadState s m, MonadIO m) => EntityID -> m ()
removeEntity entityID = do
    library <- use (ecs . wldComponentLibrary)
    forM_ library (\ComponentInterface{..} -> ciRemoveComponent entityID)
    ecs . wldEntities %= delete entityID


saveEntities :: (MonadIO m, HasECS s, MonadState s m) => m ()
saveEntities = do
    entities <- use (ecs . wldEntities)
    componentInterfaces <- Map.toList <$> use (ecs . wldComponentLibrary)
    forM_ entities $ \entityID -> do
        yaml <- foldM (\entityMap (componentName, ComponentInterface{..}) -> do
            mValue <- join <$> forM ciExtractComponent ($ entityID)
            return $ case mValue of
                Just value -> Map.insert componentName value entityMap
                Nothing -> entityMap
            ) mempty componentInterfaces
        liftIO $ print (entityID, yaml)

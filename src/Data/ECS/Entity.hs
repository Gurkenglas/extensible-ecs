{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.ECS.Entity where
import Control.Lens
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map
import System.Random
import Data.List
import Data.ECS.Types

spawnEntity :: (MonadState ECS m, MonadIO m) => ReaderT EntityID m () -> m ()
spawnEntity entityDef = do
    entityID <- newEntity
    runReaderT entityDef entityID
    registerEntity entityID

newEntity :: MonadIO m => m EntityID
newEntity = liftIO randomIO


createEntity :: (MonadState ECS m, MonadIO m) => m EntityID
createEntity = do
    entityID <- newEntity

    library  <- use wldComponentLibrary
    forM_ library (\ComponentInterface{..} -> ciAddComponent entityID)
    
    registerEntity entityID

    return entityID

registerEntity :: MonadState ECS m => EntityID -> m ()
registerEntity entityID = wldEntities %= (entityID:)

removeEntity :: (MonadState ECS m, MonadIO m) => EntityID -> m ()
removeEntity entityID = do
    library <- use wldComponentLibrary
    forM_ library (\ComponentInterface{..} -> ciRemoveComponent entityID)
    wldEntities %= delete entityID


saveEntities :: (MonadState ECS m, MonadIO m) => m ()
saveEntities = do
    entities <- use wldEntities
    componentInterfaces <- Map.toList <$> use wldComponentLibrary
    forM_ entities $ \entityID -> do
        yaml <- foldM (\entityMap (componentName, ComponentInterface{..}) -> do
            mValue <- join <$> forM ciExtractComponent ($ entityID)
            return $ case mValue of
                Just value -> Map.insert componentName value entityMap
                Nothing -> entityMap
            ) mempty componentInterfaces
        liftIO $ print (entityID, yaml)

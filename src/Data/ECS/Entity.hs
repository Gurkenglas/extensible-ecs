{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Data.ECS.Entity where
import Control.Lens
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Map (Map)
import System.Random
import Data.List
import Data.Yaml
import Text.Read
import System.FilePath
import System.Directory
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

    use wldComponentLibrary >>= mapM_ (\ComponentInterface{..} -> forM_ ciAddComponent ($ entityID))

    activateEntity entityID

    return entityID

registerEntity :: MonadState ECS m => EntityID -> m ()
registerEntity entityID = wldEntities %= (entityID:)

removeEntity :: (MonadState ECS m, MonadIO m) => EntityID -> m ()
removeEntity entityID = do
    library <- use wldComponentLibrary
    forM_ library (\ComponentInterface{..} -> ciRemoveComponent entityID)
    wldEntities %= delete entityID

deriveComponents :: (MonadIO m, MonadState ECS m) => EntityID -> m ()
deriveComponents entityID = 
    use wldComponentLibrary >>= mapM_ (\ComponentInterface{..} -> forM_ ciDeriveComponent ($ entityID))

-- | Make an entity
activateEntity :: (MonadIO m, MonadState ECS m) => EntityID -> m ()
activateEntity entityID = do
    registerEntity entityID
    deriveComponents entityID

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
        -- liftIO $ encodeFile ("my-scene" </> show entityID ++ ".yaml") yaml
        liftIO $ print (entityID, yaml)

loadScene :: (MonadIO m, MonadState ECS m) => FilePath -> m ()
loadScene sceneName = do
    entityFiles <- filter ((== ".yaml") . takeExtension) <$> liftIO (getDirectoryContents sceneName)
    forM_ entityFiles $ \entityFile -> do
        case readEither (takeBaseName entityFile) of
            Left anError -> liftIO $ putStrLn ("Error getting entityID from filename: " ++ show anError)
            Right entityID -> liftIO (decodeFileEither (sceneName </> entityFile)) >>= \case
                Left parseException -> liftIO $ putStrLn ("Error loading " ++ sceneName ++ ": " ++ show parseException)
                Right entityValue -> 
                    restoreEntity entityID entityValue
                    

restoreEntity :: (MonadIO m, MonadState ECS m) => EntityID -> Map ComponentName Value -> m ()
restoreEntity entityID entityValue = do
    componentInterfaces <- use wldComponentLibrary
    forM_ (Map.toList entityValue) $ \(componentName, value) -> 
        forM_ (Map.lookup componentName componentInterfaces) $ \ComponentInterface{..} -> 
            forM_ ciRestoreComponent $ \restoreComponent -> 
                restoreComponent value entityID
    activateEntity entityID


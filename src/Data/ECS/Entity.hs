{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Data.ECS.Entity where
import Control.Lens.Extra
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.HashMap.Strict as Map
import System.Random
import Data.List
import Data.Yaml
import Text.Read
import System.FilePath
import System.Directory
import Data.ECS.Types
import Control.Exception

data Persistence = Transient | Persistent deriving (Eq, Show)


newEntity :: MonadIO m => m EntityID
newEntity = liftIO randomIO

spawnEntity :: (MonadState ECS m, MonadIO m) => Persistence -> ReaderT EntityID m () -> m EntityID
spawnEntity persistence entityDef = do
    entityID <- newEntity
    addDefaultComponents entityID
    runReaderT entityDef entityID
    activateEntity persistence entityID
    return entityID

createEntity :: (MonadState ECS m, MonadIO m) => Persistence -> m EntityID
createEntity persistence = do
    entityID <- newEntity

    addDefaultComponents entityID

    activateEntity persistence entityID

    return entityID

addDefaultComponents :: (MonadIO m, MonadState ECS m) => EntityID -> m ()
addDefaultComponents entityID = 
    use wldComponentLibrary >>= mapM_ (\ComponentInterface{..} -> forM_ ciAddComponent (runEntity entityID))

registerEntity :: MonadState ECS m => EntityID -> m ()
registerEntity entityID = wldEntities %= (entityID:)

removeEntity :: (MonadState ECS m, MonadIO m) => EntityID -> m ()
removeEntity entityID = do
    library <- use wldComponentLibrary
    forM_ library (\ComponentInterface{..} -> runEntity entityID ciRemoveComponent)
    wldEntities %= delete entityID

deriveComponents :: (MonadIO m, MonadState ECS m) => EntityID -> m ()
deriveComponents entityID = 
    use wldComponentLibrary >>= mapM_ (\ComponentInterface{..} -> forM_ ciDeriveComponent (runEntity entityID))

-- | Registers an entity in the list of all entities, and
-- converts inert properties into live ones
activateEntity :: (MonadIO m, MonadState ECS m) => Persistence -> EntityID -> m ()
activateEntity persistence entityID = do
    when (persistence == Persistent) $ do
        registerEntity entityID
    deriveComponents entityID

saveEntities :: (MonadState ECS m, MonadIO m) => FilePath -> m ()
saveEntities sceneFolder = do
    entities <- use wldEntities
    componentInterfaces <- Map.toList <$> use wldComponentLibrary
    forM_ entities $ \entityID -> do
        yaml <- foldM (\entityMap (componentName, ComponentInterface{..}) -> do
            mValue <- join <$> forM ciExtractComponent (runEntity entityID)
            return $ case mValue of
                Just value -> Map.insert componentName value entityMap
                Nothing -> entityMap
            ) mempty componentInterfaces
        liftIO $ createDirectoryIfMissing True sceneFolder
        liftIO $ encodeFile (sceneFolder </> show entityID ++ ".yaml") yaml

getDirectoryContentsSafe :: MonadIO m => FilePath -> m [FilePath]
getDirectoryContentsSafe directory = liftIO $ catch (getDirectoryContents directory)
     (\e -> do
        putStrLn ("Error in getDirectoryContentsSafe: " ++ show (e :: IOException))
        return [])

loadEntities :: (MonadIO m, MonadState ECS m) => FilePath -> m ()
loadEntities entitiesFolder = do
    entityFiles <- filter ((== ".yaml") . takeExtension) <$> getDirectoryContentsSafe entitiesFolder
    forM_ entityFiles $ \entityFile -> do
        case readEither (takeBaseName entityFile) of
            Left anError -> liftIO $ putStrLn ("Error getting entityID from filename: " ++ show anError)
            Right entityID -> liftIO (decodeFileEither (entitiesFolder </> entityFile)) >>= \case
                Left parseException -> liftIO $ putStrLn ("Error loading " ++ (entitiesFolder </> entityFile) ++ ": " ++ show parseException)
                Right entityValue -> 
                    restoreEntity entityID entityValue

restoreEntity :: (MonadIO m, MonadState ECS m) => EntityID -> Map ComponentName Value -> m ()
restoreEntity entityID entityValue = do
    componentInterfaces <- use wldComponentLibrary
    forM_ (Map.toList entityValue) $ \(componentName, value) -> 
        forM_ (Map.lookup componentName componentInterfaces) $ \ComponentInterface{..} -> 
            forM_ ciRestoreComponent $ \restoreComponent -> 
                runEntity entityID $ restoreComponent value 
    activateEntity Persistent entityID


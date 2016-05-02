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
import System.FilePath
import System.Directory
import Data.ECS.Types
import Control.Exception

data Persistence = Transient | Persistent deriving (Eq, Show)


newEntity :: MonadIO m => m EntityID
newEntity = liftIO randomIO

spawnEntity :: (MonadState ECS m, MonadIO m) => ReaderT EntityID m () -> m EntityID
spawnEntity = spawnTransientEntity

spawnPersistentEntity :: (MonadState ECS m, MonadIO m) => ReaderT EntityID m () -> m EntityID
spawnPersistentEntity = spawnEntityWithPersistence Persistent

spawnTransientEntity :: (MonadState ECS m, MonadIO m) => ReaderT EntityID m () -> m EntityID
spawnTransientEntity = spawnEntityWithPersistence Transient

spawnEntityWithPersistence :: (MonadState ECS m, MonadIO m) => Persistence -> ReaderT EntityID m () -> m EntityID
spawnEntityWithPersistence persistence entityDef = do
    entityID <- newEntity
    runReaderT entityDef entityID
    activateEntity persistence entityID
    return entityID

registerEntity :: MonadState ECS m => EntityID -> m ()
registerEntity entityID = wldEntities %= (entityID:)

removeEntity :: (MonadState ECS m, MonadIO m) => EntityID -> m ()
removeEntity entityID = do
    library <- use wldComponentLibrary
    forM_ library (\ComponentInterface{..} -> runEntity entityID ciRemoveComponent)
    wldEntities %= delete entityID

deriveComponents :: (MonadIO m, MonadState ECS m) => EntityID -> m ()
deriveComponents entityID =
    use wldComponentLibrary >>= mapM_
        (\ComponentInterface{..} ->
            forM_ ciDeriveComponent (runEntity entityID))

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
        yaml <- entityAsJSON' entityID componentInterfaces
        liftIO $ createDirectoryIfMissing True sceneFolder
        liftIO $ encodeFile (sceneFolder </> show entityID ++ ".yaml") yaml

entityAsJSON :: (MonadIO m, MonadState ECS m) => EntityID -> m (Map ComponentName Value)
entityAsJSON entityID = do
    componentInterfaces <- Map.toList <$> use wldComponentLibrary
    entityAsJSON' entityID componentInterfaces

-- FIXME: a faster version of this could skip the serialization/deserialization
-- by simply copying values over for any component that had a ciExtractComponent
-- entry and then calling activateEntity
duplicateEntity :: (MonadIO m, MonadState ECS m) => Persistence -> EntityID -> m EntityID
duplicateEntity persistence entityID = do
    entityValues <- entityAsJSON entityID
    spawnEntityFromJSON persistence entityValues

-- | An internal version that lets us call Map.toList
-- once on the wldComponentLibrary
-- and reuse it for multiple entityAsJSON' calls
entityAsJSON' :: (MonadIO m, MonadState ECS m)
              => EntityID
              -> [(ComponentName, ComponentInterface)]
              -> m (Map ComponentName Value)
entityAsJSON' entityID componentInterfaces =
    foldM (\entityMap (componentName, ComponentInterface{..}) -> do
            mValue <- join <$> forM ciExtractComponent (runEntity entityID)
            return $ case mValue of
                Just value -> Map.insert componentName value entityMap
                Nothing    -> entityMap
            )
        mempty componentInterfaces



getDirectoryContentsSafe :: MonadIO m => FilePath -> m [FilePath]
getDirectoryContentsSafe directory = liftIO $ catch (getDirectoryContents directory)
     (\e -> do
        putStrLn ("Error in getDirectoryContentsSafe: " ++ show (e :: IOException))
        return [])

loadEntities :: (MonadIO m, MonadState ECS m) => FilePath -> m ()
loadEntities entitiesFolder = do
    entityFiles <- filter ((== ".yaml") . takeExtension) <$> getDirectoryContentsSafe entitiesFolder
    forM_ entityFiles $ \entityFile ->

        liftIO (decodeFileEither (entitiesFolder </> entityFile)) >>= \case

            Right entityValues  -> do
                let _entityName = takeBaseName entityFile
                -- TODO register entity with library here

                void $ spawnEntityFromJSON Persistent entityValues

            Left parseException ->
                liftIO $ putStrLn ("Error loading " ++ (entitiesFolder </> entityFile)
                                                    ++ ": " ++ show parseException)

spawnEntityFromJSON :: (MonadIO m, MonadState ECS m) => Persistence -> Map ComponentName Value -> m EntityID
spawnEntityFromJSON persistence entityValues = do
    entityID <- newEntity
    restoreEntityFromValues persistence entityID entityValues
    return entityID

restoreEntityFromValues :: (MonadIO m, MonadState ECS m) => Persistence -> EntityID -> Map ComponentName Value -> m ()
restoreEntityFromValues persistence entityID entityValues = do
    componentInterfaces <- use wldComponentLibrary

    runEntity entityID $
        forM_ (Map.toList entityValues) $ \(componentName, value) -> do
            forM_ (Map.lookup componentName componentInterfaces) $ \ComponentInterface{..} ->
                forM_ ciRestoreComponent $ \restoreComponent ->
                    restoreComponent value

    activateEntity persistence entityID

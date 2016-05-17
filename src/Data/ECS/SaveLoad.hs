{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Data.ECS.SaveLoad where
import Control.Lens.Extra
import Control.Monad.State
import qualified Data.HashMap.Strict as Map
import Data.Yaml
import System.FilePath
import System.Directory
import Control.Exception
import Data.ECS.Types
import Data.ECS.Entity
entityAsJSON :: (MonadIO m, MonadState ECS m) => EntityID -> m (Map ComponentName Value)
entityAsJSON entityID = do
    componentInterfaces <- Map.toList <$> use wldComponentLibrary
    entityAsJSON' entityID componentInterfaces

-- FIXME: a faster version of this could skip the serialization/deserialization
-- by simply copying values over via a ciCloneComponent function
-- (cloneComponent someKey toEntityID = setEntityComponent entityID =<< getComponent someKey)
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

getDirectoryContentsWithExtension :: MonadIO m => String -> FilePath -> m [FilePath]
getDirectoryContentsWithExtension extension folder =
    filter ((== ('.':extension)) . takeExtension) <$> getDirectoryContentsSafe folder

loadEntities :: (MonadIO m, MonadState ECS m) => FilePath -> m ()
loadEntities entitiesFolder = do
    entityFiles <- getDirectoryContentsWithExtension "yaml" entitiesFolder
    forM_ entityFiles $ \entityFile ->

        liftIO (decodeFileEither (entitiesFolder </> entityFile)) >>= \case

            Right entityValues  -> do
                let _entityName = takeBaseName entityFile
                -- TODO register entity with library here

                void $ spawnEntityFromJSON Persistent entityValues

            Left parseException ->
                liftIO $ putStrLn ("Error loading " ++ (entitiesFolder </> entityFile)
                                                    ++ ": " ++ show parseException)

spawnEntityFromJSON :: (MonadIO m, MonadState ECS m)
                    => Persistence
                    -> Map ComponentName Value
                    -> m EntityID
spawnEntityFromJSON persistence entityValues = do
    entityID <- newEntity
    restoreEntityFromValues persistence entityID entityValues
    return entityID

restoreEntityFromValues :: (MonadIO m, MonadState ECS m)
                        => Persistence
                        -> EntityID
                        -> Map ComponentName Value
                        -> m ()
restoreEntityFromValues persistence entityID entityValues = do
    componentInterfaces <- use wldComponentLibrary

    runEntity entityID $
        forM_ (Map.toList entityValues) $ \(componentName, value) -> do
            forM_ (Map.lookup componentName componentInterfaces) $
                \ComponentInterface{..} ->
                    forM_ ciRestoreComponent $ \restoreComponent ->
                        restoreComponent value

    activateEntity persistence entityID


saveEntities :: (MonadState ECS m, MonadIO m) => FilePath -> m ()
saveEntities sceneFolder = do
    entities <- use wldEntities
    componentInterfaces <- Map.toList <$> use wldComponentLibrary
    forM_ entities $ \entityID -> do
        yaml <- entityAsJSON' entityID componentInterfaces
        liftIO $ do
            createDirectoryIfMissing True sceneFolder
            encodeFile (sceneFolder </> show entityID ++ ".yaml") yaml

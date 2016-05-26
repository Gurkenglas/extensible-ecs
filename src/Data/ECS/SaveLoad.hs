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
import Text.Read

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
            mValue <- join <$> forM ciExtractComponent (inEntity entityID)
            return $ case mValue of
                Just value -> Map.insert componentName value entityMap
                Nothing    -> entityMap
            )
        mempty componentInterfaces



getDirectoryContentsSafe :: MonadIO m => FilePath -> m [FilePath]
getDirectoryContentsSafe directory = liftIO $ catch (filterSpecial <$> getDirectoryContents directory)
     (\e -> do
        putStrLn ("Error in getDirectoryContentsSafe: " ++ show (e :: IOException))
        return [])
    where filterSpecial = filter (not . (`elem` [".", ".."]))

getDirectoryContentsWithExtension :: MonadIO m => String -> FilePath -> m [FilePath]
getDirectoryContentsWithExtension extension folder =
    filter ((== ('.':extension)) . takeExtension) <$> getDirectoryContentsSafe folder

loadEntities :: (MonadIO m, MonadState ECS m) => FilePath -> m ()
loadEntities entitiesFolder = do
    entityFiles <- getDirectoryContentsWithExtension "yaml" entitiesFolder
    forM_ entityFiles (\entityFile -> loadEntityFile (entitiesFolder </> entityFile))

entityPathToEntityID :: FilePath -> Maybe EntityID
entityPathToEntityID entityPath =
    if takeExtension entityPath == ".yaml"
        then readMaybe (takeBaseName entityPath)
        else Nothing

loadEntityFile :: (MonadState ECS m, MonadIO m) => FilePath -> m ()
loadEntityFile entityPath = do
    case entityPathToEntityID entityPath of
        Just entityID ->
            liftIO (decodeFileEither entityPath) >>= \case

                Right entityValues  -> do

                    void $ spawnEntityFromJSONWithID Persistent entityID entityValues

                Left parseException ->
                    liftIO $ putStrLn ("Error loading " ++ entityPath
                                                        ++ ": " ++ show parseException)
        Nothing -> liftIO $ putStrLn ("Error loading " ++ entityPath
                                                        ++ ": couldn't determine ID")

spawnEntityFromJSON :: (MonadIO m, MonadState ECS m)
                    => Persistence
                    -> Map ComponentName Value
                    -> m EntityID
spawnEntityFromJSON persistence entityValues = do
    entityID <- newEntity
    spawnEntityFromJSONWithID persistence entityID entityValues
    return entityID

spawnEntityFromJSONWithID :: (MonadIO m, MonadState ECS m)
                          => Persistence
                          -> EntityID
                          -> Map ComponentName Value
                          -> m ()
spawnEntityFromJSONWithID persistence entityID entityValues = do
    restoreEntityFromValues persistence entityID entityValues


restoreEntityFromValues :: (MonadIO m, MonadState ECS m)
                        => Persistence
                        -> EntityID
                        -> Map ComponentName Value
                        -> m ()
restoreEntityFromValues persistence entityID entityValues = do
    componentInterfaces <- use wldComponentLibrary

    inEntity entityID $
        forM_ (Map.toList entityValues) $ \(componentName, value) -> do
            forM_ (Map.lookup componentName componentInterfaces) $
                \ComponentInterface{..} ->
                    forM_ ciRestoreComponent $ \restoreComponent ->
                        restoreComponent value

    activateEntity persistence entityID


saveEntities :: (MonadState ECS m, MonadIO m) => FilePath -> m ()
saveEntities sceneFolder = do
    entities <- use wldPersistentEntities
    componentInterfaces <- Map.toList <$> use wldComponentLibrary
    liftIO $ createDirectoryIfMissing True sceneFolder
    forM_ entities $ \entityID -> do
        saveEntity' entityID sceneFolder componentInterfaces

saveEntity :: (MonadState ECS m, MonadIO m) => EntityID -> FilePath -> m ()
saveEntity entityID sceneFolder = do
    componentInterfaces <- Map.toList <$> use wldComponentLibrary
    liftIO $ createDirectoryIfMissing True sceneFolder
    saveEntity' entityID sceneFolder componentInterfaces

saveEntity' :: (MonadIO m, MonadState ECS m)
            => EntityID -> FilePath -> [(ComponentName, ComponentInterface)] -> m ()
saveEntity' entityID sceneFolder componentInterfaces = do
    yaml <- entityAsJSON' entityID componentInterfaces
    liftIO $ encodeFile (pathForEntity sceneFolder entityID) yaml

pathForEntity sceneFolder entityID = (sceneFolder </> show entityID ++ ".yaml")

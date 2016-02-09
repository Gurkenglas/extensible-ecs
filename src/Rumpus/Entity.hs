{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Rumpus.Entity where
import Control.Lens
import Prelude hiding (lookup)
import Control.Monad.State
import qualified Data.Map as Map
import System.Random
import Data.List
import Rumpus.Types


newEntity :: MonadIO m => m EntityID
newEntity = liftIO randomIO


createEntity :: WorldMonad EntityID
createEntity = do
    entityID <- newEntity
    library <- use wldComponentLibrary
    forM_ library (\ComponentInterface{..} -> ciAddComponent entityID)
    wldEntities %= (entityID:)

    return entityID


removeEntity :: EntityID -> WorldMonad ()
removeEntity entityID = do
    library <- use wldComponentLibrary
    forM_ library (\ComponentInterface{..} -> ciRemoveComponent entityID)
    wldEntities %= delete entityID


saveEntities :: WorldMonad ()
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

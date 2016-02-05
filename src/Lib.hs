{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Lib where
import qualified Data.Vault.Strict as Vault
import Data.Vault.Strict (Vault, Key)
import Control.Lens
import Prelude hiding (lookup)
import Control.Monad.State
import           Data.Map (Map)
import qualified Data.Map as Map
import GHC.Word
import System.Random
import Data.Yaml hiding ((.=))

type EntityID = Word32

type EntityMap a = Map EntityID a

type ComponentName = String

type WorldMonad a = StateT World IO a

data ComponentInterface = ComponentInterface
    { ciAddComponent :: (EntityID -> WorldMonad ())
    , ciExtractComponent :: (EntityID -> WorldMonad (Maybe Value))
    }

data World = World
    { _wldSystems          :: Vault
    , _wldComponents       :: Vault
    , _wldComponentLibrary :: Map ComponentName ComponentInterface
    , _wldEntities         :: [EntityID]
    }
makeLenses ''World

newEntity :: MonadIO m => m EntityID
newEntity = liftIO randomIO

createEntity :: StateT World IO ()
createEntity = do
    entityID <- newEntity
    library <- use wldComponentLibrary
    forM_ library (\ComponentInterface{..} -> ciAddComponent entityID)
    wldEntities %= (entityID:)

newWorld :: World
newWorld = World mempty mempty mempty mempty

withSystem :: MonadState World m => Key a -> (a -> m b) -> m ()
withSystem systemKey action = do
    systems <- use wldSystems
    forM_ (Vault.lookup systemKey systems) action



addComponentToEntity :: (MonadIO m,MonadState World m) => Key (EntityMap a) -> EntityID -> a -> m ()
addComponentToEntity componentKey entityID value = 
    withComponentMap_ componentKey $ \componentMap -> do
        componentMaps <- use wldComponents
        let newComponentMap = Map.insert entityID value componentMap
            newComponentMaps = Vault.insert componentKey newComponentMap componentMaps
        wldComponents .= newComponentMaps

registerComponentType :: MonadState World m => String -> ComponentInterface -> m ()
registerComponentType name addFunction = wldComponentLibrary . at name ?= addFunction

withComponentMap_ :: MonadState World m => Key (EntityMap a) -> ((EntityMap a) -> m b) -> m ()
withComponentMap_ componentKey = void . withComponentMap componentKey

withComponentMap :: MonadState World m => Key (EntityMap a) -> ((EntityMap a) -> m b) -> m (Maybe b)
withComponentMap componentKey action = do
    componentMaps <- use wldComponents
    forM (Vault.lookup componentKey componentMaps) action

traverseComponentEntities :: MonadState World m => Key (EntityMap a) -> ((EntityID, a) -> m b) -> m ()
traverseComponentEntities componentKey action = 
    withComponentMap_ componentKey $ \componentMap -> 
        forM_ (Map.toList componentMap) action

-- saveEntities :: (MonadState World m, MonadIO m) => m ()
saveEntities :: WorldMonad ()
saveEntities = do
    entities <- use wldEntities
    componentInterfaces <- Map.toList <$> use wldComponentLibrary
    forM_ entities $ \entityID -> do
        yaml <- foldM (\entityMap (componentName, ComponentInterface{..}) -> do
            mValue <- ciExtractComponent entityID
            return $ case mValue of
                Just value -> Map.insert componentName value entityMap
                Nothing -> entityMap
            ) mempty componentInterfaces
        liftIO $ print (entityID, yaml)

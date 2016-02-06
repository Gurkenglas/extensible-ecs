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
import Language.Haskell.TH
import System.IO.Unsafe
import Data.Char
import Data.List
{-
Generates definitions of the form:
{-# NOINLINE soundSystemKey #-}
soundSystemKey :: Key SoundSystem
soundSystemKey = unsafePerformIO newKey
-}
defineKey :: String -> TypeQ -> DecsQ
defineKey keyString keyType = sequence [inlineDecl, signatureDecl, keyDecl]
    where
        keyName       = mkName keyString
        inlineDecl    = pragInlD keyName NoInline FunLike AllPhases
        signatureDecl = sigD keyName (conT ''Key `appT` keyType)
        keyDecl       = valD (varP keyName) (normalB (appE (varE 'unsafePerformIO) (varE 'Vault.newKey))) []

defineSystemKey :: Name -> DecsQ
defineSystemKey name = do
    let systemKeyName = toKeyName (nameBase name)
    defineKey systemKeyName (conT name)


defineComponentKey :: Name -> DecsQ
defineComponentKey name = do
    let componentKeyName = toKeyName (nameBase name)
    defineKey componentKeyName (conT ''EntityMap `appT` conT name)


toKeyName :: String -> String
toKeyName (x:xs) = toLower x:xs ++ "Key"
toKeyName [] = error "toKeyName: empty Key type name"

type EntityID = Word32

type EntityMap a = Map EntityID a

type ComponentName = String

type WorldMonad a = StateT World IO a

data ComponentInterface = ComponentInterface
    { ciAddComponent     :: (EntityID -> WorldMonad ())
    , ciExtractComponent :: (EntityID -> WorldMonad (Maybe Value))
    , ciRemoveComponent  :: (EntityID -> WorldMonad ())
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
    wldEntities %= (delete entityID)

newWorld :: World
newWorld = World mempty mempty mempty mempty

withSystem :: MonadState World m => Key a -> (a -> m b) -> m ()
withSystem systemKey action = do
    systems <- use wldSystems
    forM_ (Vault.lookup systemKey systems) action



addComponentToEntity :: (MonadIO m,MonadState World m) => Key (EntityMap a) -> a -> EntityID-> m ()
addComponentToEntity componentKey value entityID = 
    withComponentMap_ componentKey $ \componentMap -> do
        componentMaps <- use wldComponents
        let newComponentMap = Map.insert entityID value componentMap
            newComponentMaps = Vault.insert componentKey newComponentMap componentMaps
        wldComponents .= newComponentMaps

removeComponentFromEntity :: (MonadIO m,MonadState World m) => Key (EntityMap a) -> EntityID -> m ()
removeComponentFromEntity componentKey entityID = 
    withComponentMap_ componentKey $ \componentMap -> do
        componentMaps <- use wldComponents
        let newComponentMap = Map.delete entityID componentMap
            newComponentMaps = Vault.insert componentKey newComponentMap componentMaps
        wldComponents .= newComponentMaps

registerSystem :: MonadState World m => Key a -> a -> m ()
registerSystem systemKey system = wldSystems %= Vault.insert systemKey system

registerComponentType :: MonadState World m => String -> Key (EntityMap a) -> ComponentInterface -> m ()
registerComponentType name componentKey addFunction = do
    wldComponents %= Vault.insert componentKey mempty
    wldComponentLibrary . at name ?= addFunction

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

getComponentJSON :: (MonadState World f, ToJSON a) => Key (EntityMap a) -> EntityID -> f (Maybe Value)
getComponentJSON componentKey entityID = fmap toJSON <$> getComponent entityID componentKey

getComponent :: MonadState World f => EntityID -> Key (EntityMap a) -> f (Maybe a)
getComponent entityID componentKey = 
    fmap join $ withComponentMap componentKey $ \componentMap ->
        return $ Map.lookup entityID componentMap

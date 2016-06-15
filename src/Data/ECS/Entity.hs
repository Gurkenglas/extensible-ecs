{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Data.ECS.Entity where
import Control.Lens.Extra
import Control.Monad.State
import Control.Monad.Reader
import System.Random
import Data.ECS.Types
import qualified Data.HashSet as Set

data Persistence = Transient | Persistent deriving (Eq, Show)


newEntityID :: MonadIO m => m EntityID
newEntityID = liftIO randomIO

spawnEntity :: (MonadState ECS m, MonadIO m) => ReaderT EntityID m () -> m EntityID
spawnEntity = spawnTransientEntity

spawnEntity_ :: (MonadState ECS m, MonadIO m) => ReaderT EntityID m () -> m ()
spawnEntity_ = void . spawnEntity

spawnPersistentEntity :: (MonadState ECS m, MonadIO m) => ReaderT EntityID m () -> m EntityID
spawnPersistentEntity = spawnEntityWithPersistence Persistent

spawnTransientEntity :: (MonadState ECS m, MonadIO m) => ReaderT EntityID m () -> m EntityID
spawnTransientEntity = spawnEntityWithPersistence Transient

spawnEntityWithPersistence :: (MonadState ECS m, MonadIO m)
                           => Persistence
                           -> ReaderT EntityID m ()
                           -> m EntityID
spawnEntityWithPersistence persistence entityDef = do
    entityID <- newEntityID
    runReaderT entityDef entityID
    activateEntity persistence entityID
    return entityID


isEntityPersistent :: MonadState ECS m => EntityID -> m Bool
isEntityPersistent entityID = Set.member entityID <$> use wldPersistentEntities

makeEntityPersistent :: MonadState ECS m => EntityID -> m ()
makeEntityPersistent entityID = wldPersistentEntities %= (Set.insert entityID)

removeEntity :: (MonadState ECS m, MonadIO m) => EntityID -> m ()
removeEntity entityID = do
    library <- use wldComponentLibrary
    forM_ library (\ComponentInterface{..} -> inEntity entityID ciRemoveComponent)
    wldPersistentEntities %= Set.delete entityID

deriveComponents :: (MonadIO m, MonadState ECS m) => EntityID -> m ()
deriveComponents entityID =
    use wldComponentLibrary >>= mapM_
        (\ComponentInterface{..} ->
            forM_ ciDeriveComponent (inEntity entityID))

-- | Registers an entity in the list of all entities, and
-- converts inert properties into live ones
activateEntity :: (MonadIO m, MonadState ECS m) => Persistence -> EntityID -> m ()
activateEntity persistence entityID = do
    when (persistence == Persistent) $ do
        makeEntityPersistent entityID
    deriveComponents entityID

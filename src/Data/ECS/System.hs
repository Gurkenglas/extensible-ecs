{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
module Data.ECS.System where
import qualified Data.ECS.Vault as Vault
import Data.ECS.Vault (Key)
import Control.Lens.Extra
import Control.Monad.State
import Data.Maybe
import Data.ECS.Types
import Data.Monoid

registerSystem :: MonadState ECS m => Key a -> a -> m ()
registerSystem = setSystem

setSystem :: MonadState ECS m => Key a -> a -> m ()
setSystem systemKey system = wldSystems . vault systemKey ?= system

withSystem :: MonadState ECS m => Key a -> (a -> m b) -> m (Maybe b)
withSystem systemKey action = do
    systems <- preuse $ wldSystems . vault systemKey
    forM systems action

wldSystem :: Key s -> Lens' ECS s
wldSystem systemKey = wldSystems . vault systemKey . singular _Just

--viewSystem :: MonadState ECS m => Key s -> Lens' s a -> m a
viewSystem systemKey viewLens = use $ wldSystem systemKey . viewLens

--viewSystemL :: MonadState ECS m => Key s -> Getting (Endo [a]) s a -> m [a]
viewSystemL systemKey viewFold = gets $ toListOf $ wldSystem systemKey . viewFold

--viewSystemP :: MonadState ECS m => Key s -> Getting (First a) s a -> m (Maybe a)
viewSystemP systemKey viewTraversal = gets $ preview $ wldSystem systemKey . viewTraversal

withSystem_ :: MonadState ECS m => Key a -> (a -> m b) -> m ()
withSystem_ systemKey = void . withSystem systemKey

modifySystem :: MonadState ECS m => Key s -> (s -> m (s, b)) -> m b
modifySystem systemKey action = do
    system <- use $ wldSystem systemKey
    (newSystem, b) <- action system
    setSystem systemKey newSystem
    return b

modifySystem_ :: MonadState ECS m => Key a -> (a -> m a) -> m ()
modifySystem_ systemKey action = void $ modifySystem systemKey (fmap (,()) . action)

modifySystemState :: MonadState ECS m => Key s -> StateT s m b -> m b
modifySystemState systemKey action = modifySystem systemKey $ \system -> do
    (r, newState) <- runStateT action system
    return (newState, r)

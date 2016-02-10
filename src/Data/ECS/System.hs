{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Data.ECS.System where
import qualified Data.Vault.Strict as Vault
import Data.Vault.Strict (Key)
import Control.Lens
import Prelude hiding (lookup)
import Control.Monad.State

import Data.ECS.Types

registerSystem :: (HasECS s, MonadState s m) => Key a -> a -> m ()
registerSystem systemKey system = ecs . wldSystems %= Vault.insert systemKey system

withSystem :: (HasECS s, MonadState s m) => Key a -> (a -> m b) -> m ()
withSystem systemKey action = do
    systems <- use (ecs . wldSystems)
    forM_ (Vault.lookup systemKey systems) action

modifySystem :: (HasECS s, MonadState s m) => Key a -> (a -> m a) -> m ()
modifySystem systemKey action = 
    withSystem systemKey $ \system -> do
        newSystem <- action system
        ecs . wldSystems %= Vault.insert systemKey newSystem

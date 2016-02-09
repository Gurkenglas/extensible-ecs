{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Rumpus.System where
import qualified Data.Vault.Strict as Vault
import Data.Vault.Strict (Key)
import Control.Lens
import Prelude hiding (lookup)
import Control.Monad.State

import Rumpus.Types

registerSystem :: MonadState World m => Key a -> a -> m ()
registerSystem systemKey system = wldSystems %= Vault.insert systemKey system

withSystem :: MonadState World m => Key a -> (a -> m b) -> m ()
withSystem systemKey action = do
    systems <- use wldSystems
    forM_ (Vault.lookup systemKey systems) action

modifySystem :: MonadState World m => Key a -> (a -> m a) -> m ()
modifySystem systemKey action = 
    withSystem systemKey $ \system -> do
        newSystem <- action system
        wldSystems %= Vault.insert systemKey newSystem

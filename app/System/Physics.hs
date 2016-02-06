{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Physics where
import Data.Vault.Strict
import Control.Monad.State
import Control.Lens
import Lib
import Prelude hiding (lookup)


data PhysicsSystem = PhysicsSystem Int deriving Show
defineSystemKey ''PhysicsSystem

data PhysicsComponent = PhysicsComponent { _pcMass :: Float } deriving Show
defineComponentKey ''PhysicsComponent

initSystemPhysics :: MonadState World m => m ()
initSystemPhysics = do
    wldSystems %= insert physicsSystemKey (PhysicsSystem 0)

tickSystemPhysics :: (MonadState World m, MonadIO m) => m ()
tickSystemPhysics = do
    
    return ()


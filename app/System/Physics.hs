{-# LANGUAGE FlexibleContexts #-}
module System.Physics where
import Data.Vault.Strict
import Control.Monad.State
import System.IO.Unsafe
import Control.Lens
import Lib
import Prelude hiding (lookup)

-- import System.Color
-- import System.Sound

{-# NOINLINE physicsSystemKey #-}
physicsSystemKey :: Key PhysicsSystem
physicsSystemKey = unsafePerformIO newKey

{-# NOINLINE physicsComponentKey #-}
physicsComponentKey :: Key (EntityMap PhysicsComponent)
physicsComponentKey = unsafePerformIO newKey

data PhysicsSystem = PhysicsSystem Int deriving Show

data PhysicsComponent = PhysicsComponent { _pcMass :: Float } deriving Show



initSystemPhysics :: MonadState World m => m ()
initSystemPhysics = do
    wldSystems %= insert physicsSystemKey (PhysicsSystem 0)

tickSystemPhysics :: (MonadState World m, MonadIO m) => m ()
tickSystemPhysics = do
    
    return ()


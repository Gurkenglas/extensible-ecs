{-# LANGUAGE FlexibleContexts #-}
module System.Sound where
import Data.Vault.Strict
import Control.Monad.State
import System.IO.Unsafe
import Control.Lens
import Lib
import Prelude hiding (lookup)

{-# NOINLINE soundSystemKey #-}
soundSystemKey :: Key SoundSystem
soundSystemKey = unsafePerformIO newKey

{-# NOINLINE soundComponentKey #-}
soundComponentKey :: Key (EntityMap SoundComponent)
soundComponentKey = unsafePerformIO newKey



data SoundSystem = SoundSystem Int deriving Show

data SoundComponent = SoundComponent 
    { scChannel  :: Int
    , scSourceID :: Int 
    } deriving Show



initSystemSound :: MonadState World m => m ()
initSystemSound = do
    wldSystems %= insert soundSystemKey (SoundSystem 0)

tickSystemSound :: (MonadState World m, MonadIO m) => m ()
tickSystemSound = withSystem soundSystemKey $ \(SoundSystem i) -> do
    let newValue = (SoundSystem (i + 1))
    liftIO . print $ newValue
    wldSystems %= insert soundSystemKey newValue


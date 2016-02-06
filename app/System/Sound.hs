{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Sound where
import Data.Vault.Strict
import Control.Monad.State
import Control.Lens
import Lib
import Prelude hiding (lookup)

data SoundSystem = SoundSystem Int deriving Show
defineSystemKey ''SoundSystem

data SoundComponent = SoundComponent 
    { scChannel  :: Int
    , scSourceID :: Int 
    } deriving Show
defineComponentKey ''SoundComponent


initSystemSound :: MonadState World m => m ()
initSystemSound = do
    wldSystems %= insert soundSystemKey (SoundSystem 0)

tickSystemSound :: (MonadState World m, MonadIO m) => m ()
tickSystemSound = withSystem soundSystemKey $ \(SoundSystem i) -> do
    let newValue = (SoundSystem (i + 1))
    liftIO . print $ newValue
    wldSystems %= insert soundSystemKey newValue


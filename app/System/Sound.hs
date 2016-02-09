{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Sound where
import Control.Monad.State
import Rumpus

data SoundSystem = SoundSystem Int deriving Show
defineSystemKey ''SoundSystem

data SoundSource = SoundSource 
    { scChannel  :: Int
    , scSourceID :: Int 
    } deriving Show
defineComponentKey ''SoundSource


initSystemSound :: MonadState World m => m ()
initSystemSound = do
    registerSystem soundSystemKey (SoundSystem 0)

tickSystemSound :: (MonadState World m, MonadIO m) => m ()
tickSystemSound = modifySystem soundSystemKey $ \(SoundSystem i) -> do
    let newValue = (SoundSystem (i + 1))
    liftIO . print $ newValue
    return newValue


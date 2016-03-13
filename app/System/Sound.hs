{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module System.Sound where
import Control.Monad.State
import Data.ECS
import Control.Lens.Extra
import Data.Yaml
import GHC.Generics

data SoundSystem = SoundSystem { _ssBlah :: Int } deriving Show
defineSystemKey ''SoundSystem
makeLenses ''SoundSystem

data SoundSource = SoundSource 
    { scChannel  :: Int
    , scSourceID :: Int 
    } deriving (Show, Generic, FromJSON, ToJSON)
defineComponentKey ''SoundSource


initSystemSound :: (MonadState ECS m) => m ()
initSystemSound = do
    registerSystem sysSound (SoundSystem 0)
    registerComponent "SoundSource" cmpSoundSource (defaultComponentInterface cmpSoundSource (SoundSource 100 1000))

tickSystemSound :: (MonadState ECS m, MonadIO m) => m ()
tickSystemSound = modifySystemState sysSound $ do
    newValue <- ssBlah <+= 1
    liftIO . print $ newValue


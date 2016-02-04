{-# LANGUAGE TemplateHaskell #-}
module Lib where
import Data.Vault.Strict
import Control.Lens

data WorldState = WorldState
    { _wlsData    :: Vault
    , _wlsSystems :: Vault
    }
makeLenses ''WorldState

newWorld :: WorldState
newWorld = WorldState mempty mempty

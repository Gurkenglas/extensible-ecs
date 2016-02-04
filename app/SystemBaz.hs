{-# LANGUAGE FlexibleContexts #-}
module SystemBaz where
import Data.Vault.Strict
import Control.Monad.State
import System.IO.Unsafe
import Control.Lens
import Lib
import Prelude hiding (lookup)

import SystemFoo
import SystemBar

data DataBaz = DataBaz Int deriving Show

{-# NOINLINE bazKey #-}
bazKey :: Key DataBaz
bazKey = unsafePerformIO newKey

initSystemBaz :: MonadState WorldState m => m ()
initSystemBaz = do
    wlsData %= insert bazKey (DataBaz 0)

tickSystemBaz :: (MonadState WorldState m, MonadIO m) => m ()
tickSystemBaz = do
    worldData <- use wlsData
    forM_ (lookup bazKey worldData) $ \(DataBaz i) -> do
        let newValue = (DataBaz (i + 1))
        liftIO . print $ newValue
        wlsData %= (insert bazKey newValue)
    forM_ (lookup fooKey worldData) $ \(DataFoo i) -> do
        let newValue = (DataFoo (i + 1))
        liftIO . print $ newValue
        wlsData %= (insert fooKey newValue)
    forM_ (lookup barKey worldData) $ \(DataBar i) -> do
        let newValue = (DataBar (i + 1))
        liftIO . print $ newValue
        wlsData %= (insert barKey newValue)


{-# LANGUAGE FlexibleContexts #-}
module SystemBar where
import Data.Vault.Strict
import Control.Monad.State
import System.IO.Unsafe
import Control.Lens
import Lib
import Prelude hiding (lookup)

data DataBar = DataBar Int deriving Show

{-# NOINLINE barKey #-}
barKey :: Key DataBar
barKey = unsafePerformIO newKey

initSystemBar :: MonadState WorldState m => m ()
initSystemBar = do
    wlsData %= insert barKey (DataBar 0)

tickSystemBar :: (MonadState WorldState m, MonadIO m) => m ()
tickSystemBar = do
    worldData <- use wlsData
    forM_ (lookup barKey worldData) $ \(DataBar i) -> do
        let newValue = (DataBar (i + 1))
        liftIO . print $ newValue
        wlsData %= (insert barKey newValue)


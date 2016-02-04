{-# LANGUAGE FlexibleContexts #-}
module SystemFoo where
import Data.Vault.Strict
import Control.Monad.State
import System.IO.Unsafe
import Control.Lens
import Lib
import Prelude hiding (lookup)

data DataFoo = DataFoo Int deriving Show

{-# NOINLINE fooKey #-}
fooKey :: Key DataFoo
fooKey = unsafePerformIO newKey

initSystemFoo :: MonadState WorldState m => m ()
initSystemFoo = do
    wlsData %= insert fooKey (DataFoo 0)

tickSystemFoo :: (MonadState WorldState m, MonadIO m) => m ()
tickSystemFoo = do
    worldData <- use wlsData
    forM_ (lookup fooKey worldData) $ \(DataFoo i) -> do
        let newValue = (DataFoo (i + 1))
        liftIO . print $ newValue
        wlsData %= (insert fooKey newValue)


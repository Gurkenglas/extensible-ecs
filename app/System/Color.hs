{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
module System.Color where
import Data.Vault.Strict
import Control.Monad.State
import System.IO.Unsafe
import Control.Lens
import Lib
import Prelude hiding (lookup)
import System.Random
-- import           Data.Map (Map)
import qualified Data.Map as Map
import Data.Yaml
import GHC.Generics

{-# NOINLINE colorSystemKey #-}
colorSystemKey :: Key ColorSystem
colorSystemKey = unsafePerformIO newKey

{-# NOINLINE colorComponentKey #-}
colorComponentKey :: Key (EntityMap ColorComponent)
colorComponentKey = unsafePerformIO newKey

type ColorOption = String

data ColorSystem = ColorSystem { csColorOptions :: [ColorOption] } deriving Show

data ColorComponent = ColorComponent ColorOption deriving (Show, Generic, ToJSON)

newColorSystem :: ColorSystem
newColorSystem = ColorSystem ["red", "blue", "green"]

addColorComponent :: (MonadIO m, MonadState World m) => EntityID -> m ()
addColorComponent entityID = withSystem colorSystemKey $ \(ColorSystem options) -> do
    randomColorIdx <- liftIO (randomRIO (0, length options - 1))
    let chosenColor = options !! randomColorIdx
    addComponentToEntity colorComponentKey entityID (ColorComponent chosenColor)

getColorComponent :: MonadState World f => EntityID -> f (Maybe Value)
getColorComponent entityID = do
    fmap join $ withComponentMap colorComponentKey $ \componentMap ->
        return $ toJSON <$> Map.lookup entityID componentMap

initSystemColor :: MonadState World m => m ()
initSystemColor = do
    wldSystems %= insert colorSystemKey newColorSystem
    wldComponents %= insert colorComponentKey mempty

    registerComponentType "Color" (ComponentInterface addColorComponent getColorComponent)

tickSystemColor :: (MonadState World m, MonadIO m) => m ()
tickSystemColor = do
    traverseComponentEntities colorComponentKey $ \(entityID, color) ->
        liftIO (print (entityID, color))
    return ()



 

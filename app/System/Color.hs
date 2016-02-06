{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Color where
import Control.Monad.State
import Lib
import Prelude hiding (lookup)
import System.Random
import Data.Yaml
import GHC.Generics

type ColorOption = String

data ColorSystem = ColorSystem { csColorOptions :: [ColorOption] } deriving Show

data ColorComponent = ColorComponent ColorOption deriving (Show, Generic, ToJSON)

defineSystemKey ''ColorSystem
defineComponentKey ''ColorComponent

initSystemColor :: MonadState World m => m ()
initSystemColor = do
    
    registerSystem colorSystemKey newColorSystem

    registerComponentType "Color" colorComponentKey $ ComponentInterface 
        { ciAddComponent     = addColorComponent
        , ciExtractComponent = getComponentJSON colorComponentKey 
        , ciRemoveComponent  = removeComponentFromEntity colorComponentKey
        }

newColorSystem :: ColorSystem
newColorSystem = ColorSystem ["red", "blue", "green"]

addColorComponent :: (MonadIO m, MonadState World m) => EntityID -> m ()
addColorComponent entityID = withSystem colorSystemKey $ \(ColorSystem options) -> do
    randomColorIdx <- liftIO (randomRIO (0, length options - 1))
    let chosenColor = options !! randomColorIdx
    addComponentToEntity colorComponentKey (ColorComponent chosenColor) entityID

tickSystemColor :: (MonadState World m, MonadIO m) => m ()
tickSystemColor = do
    traverseComponentEntities colorComponentKey $ \(entityID, color) ->
        liftIO (print (entityID, color))
    return ()


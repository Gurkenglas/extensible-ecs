{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Color where
import Control.Monad.State
import Data.ECS
import System.Random
import Data.Yaml
import GHC.Generics

type ColorOption = String

data ColorSystem = ColorSystem { csColorOptions :: [ColorOption] } deriving Show
defineSystemKey ''ColorSystem

data Color = Color ColorOption deriving (Show, Generic, ToJSON)
defineComponentKey ''Color

initSystemColor :: (MonadIO m, MonadState ECS m) => m ()
initSystemColor = do
    
    registerSystem sysColor newColorSystem

    registerComponent "Color" cmpColor $ ComponentInterface 
        { ciAddComponent     = \entityID -> withSystem_ sysColor $ \(ColorSystem options) -> do
                randomColorIdx <- liftIO (randomRIO (0, length options - 1))
                let chosenColor = options !! randomColorIdx
                addComponent cmpColor (Color chosenColor) entityID
        , ciExtractComponent = Just (getComponentJSON cmpColor)
        , ciRemoveComponent  = removeComponent cmpColor
        }

newColorSystem :: ColorSystem
newColorSystem = ColorSystem ["red", "blue", "green"]

tickSystemColor :: (MonadState ECS m, MonadIO m) => m ()
tickSystemColor = do
    forEntitiesWithComponent cmpColor $ \(entityID, color) ->
        liftIO (print (entityID, color))
    return ()


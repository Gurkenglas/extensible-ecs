{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Color where
import Control.Monad.State
import Rumpus
import System.Random
import Data.Yaml
import GHC.Generics

type ColorOption = String

data ColorSystem = ColorSystem { csColorOptions :: [ColorOption] } deriving Show
defineSystemKey ''ColorSystem

data Color = Color ColorOption deriving (Show, Generic, ToJSON)
defineComponentKey ''Color

initSystemColor :: (MonadIO m, HasECS s, MonadState s m) => m ()
initSystemColor = do
    
    registerSystem colorSystemKey newColorSystem

    registerComponent "Color" colorKey $ ComponentInterface 
        { ciAddComponent     = \entityID -> withSystem colorSystemKey $ \(ColorSystem options) -> do
                randomColorIdx <- liftIO (randomRIO (0, length options - 1))
                let chosenColor = options !! randomColorIdx
                addComponentToEntity colorKey (Color chosenColor) entityID
        , ciExtractComponent = Just (getComponentJSON colorKey)
        , ciRemoveComponent  = removeComponentFromEntity colorKey
        }

newColorSystem :: ColorSystem
newColorSystem = ColorSystem ["red", "blue", "green"]

tickSystemColor :: (HasECS s, MonadState s m, MonadIO m) => m ()
tickSystemColor = do
    traverseEntitiesWithComponent colorKey $ \(entityID, color) ->
        liftIO (print (entityID, color))
    return ()


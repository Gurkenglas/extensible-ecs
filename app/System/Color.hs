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

data Color = Color ColorOption deriving (Show, Generic, ToJSON, FromJSON)
defineComponentKey ''Color

initSystemColor :: (MonadIO m, MonadState ECS m) => m ()
initSystemColor = do
    
    registerSystem sysColor (ColorSystem ["red", "blue", "green"])

    registerComponent "Color" cmpColor $ ComponentInterface 
        { ciAddComponent     = Just $ withSystem_ sysColor $ \(ColorSystem options) -> do
                randomColorIdx <- liftIO (randomRIO (0, length options - 1))
                let chosenColor = options !! randomColorIdx
                addComponent cmpColor (Color chosenColor)

        , ciExtractComponent = Just (getComponentJSON cmpColor)
        , ciRestoreComponent = Just (setComponentJSON cmpColor)
        , ciDeriveComponent  = Nothing
        , ciRemoveComponent  = removeComponent cmpColor
        }


tickSystemColor :: (MonadState ECS m, MonadIO m) => m ()
tickSystemColor = do
    forEntitiesWithComponent cmpColor $ \(entityID, color) ->
        liftIO (print (entityID, color))
    return ()


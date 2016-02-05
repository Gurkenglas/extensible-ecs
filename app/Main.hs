module Main where
import Control.Monad.State
import Lib

import System.Color
import System.Physics
import System.Sound

main :: IO ()
main = do
    void . flip runStateT newWorld $ do
        initSystemColor
        initSystemPhysics
        initSystemSound


        createEntity
        createEntity
        createEntity
        replicateM_ 10 $ do
            tickSystemColor
            tickSystemPhysics
            tickSystemSound
        saveEntities




{-# LANGUAGE FlexibleContexts #-}
module Main where
import Control.Monad.State
import Data.ECS

import System.Color
import System.Physics
import System.Sound

main :: IO ()
main = do
    void . flip runStateT newECS $ do
        initSystemColor
        initSystemPhysics
        initSystemSound

        loadEntities "my-scene"
        saveEntities "my-scene"
        -- let entityDef = do
        --         cmpColor       ==> (Color "Cheese")
        --         cmpSoundSource ==> (SoundSource 1 2)


        -- -- spawnEntity Persistent entityDef
        -- -- spawnEntity Persistent entityDef
        -- -- spawnEntity Persistent entityDef

        -- -- entityX <- createEntity Persistent
        -- -- _ <- createEntity Persistent
        -- -- _ <- createEntity Persistent
        -- -- replicateM_ 10 $ do
        -- --     tickSystemColor
        -- --     tickSystemPhysics
        -- --     tickSystemSound

        -- -- liftIO (putStrLn "Entities:")
        -- -- saveEntities

        -- -- liftIO (putStrLn "Entities after remove:")
        -- -- removeEntity entityX
        
        -- -- saveEntities



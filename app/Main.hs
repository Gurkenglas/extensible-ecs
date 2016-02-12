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

        loadScene "my-scene"
        saveEntities
        -- let entityDef = do
        --         cmpColor       ==> (Color "Cheese")
        --         cmpSoundSource ==> (SoundSource 1 2)


        -- -- spawnEntity entityDef
        -- -- spawnEntity entityDef
        -- -- spawnEntity entityDef

        -- -- entityX <- createEntity
        -- -- _ <- createEntity
        -- -- _ <- createEntity
        -- -- replicateM_ 10 $ do
        -- --     tickSystemColor
        -- --     tickSystemPhysics
        -- --     tickSystemSound

        -- -- liftIO (putStrLn "Entities:")
        -- -- saveEntities

        -- -- liftIO (putStrLn "Entities after remove:")
        -- -- removeEntity entityX
        
        -- -- saveEntities



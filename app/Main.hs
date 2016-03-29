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

        liftIO $ print cmpMass
        liftIO $ print cmpRigidBody
        liftIO $ print cmpColor
        liftIO $ print cmpSoundSource
        --loadEntities "my-scene"
        let entityDef = do
                cmpShapeType   ==> CubeShape
                cmpColor       ==> Color "Cheese"
                cmpSoundSource ==> SoundSource 1 2


        prototypeEntityID <- spawnEntity Persistent entityDef
        dupeEntityID <- duplicateEntity Persistent prototypeEntityID

        _ <- spawnEntity Persistent entityDef
        _ <- spawnEntity Persistent entityDef
        
        liftIO . print =<< entityAsJSON dupeEntityID
        
        saveEntities "my-scene"

        -- -- replicateM_ 10 $ do
        -- --     tickSystemColor
        -- --     tickSystemPhysics
        -- --     tickSystemSound

        -- -- liftIO (putStrLn "Entities:")
        -- -- saveEntities

        -- -- liftIO (putStrLn "Entities after remove:")
        -- -- removeEntity dupeEntityID
        
        -- -- saveEntities



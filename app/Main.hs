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

        liftIO $ print myMass
        liftIO $ print myRigidBody
        liftIO $ print myColor
        liftIO $ print mySoundSource
        --loadEntities "my-scene"
        let entityDef = do
                myShapeType   ==> CubeShape
                myColor       ==> Color "Cheese"
                mySoundSource ==> SoundSource 1 2


        prototypeEntityID <- spawnPersistentEntity entityDef
        dupeEntityID <- duplicateEntity Persistent prototypeEntityID

        _ <- spawnPersistentEntity entityDef
        _ <- spawnPersistentEntity entityDef
        
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



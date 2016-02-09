module Main where
import Control.Monad.State
import Rumpus

import System.Color
import System.Physics
import System.Sound

main :: IO ()
main = do
    void . flip runStateT newWorld $ do
        initSystemColor
        initSystemPhysics
        initSystemSound


        entity1 <- createEntity
        _ <- createEntity
        _ <- createEntity
        replicateM_ 10 $ do
            tickSystemColor
            tickSystemPhysics
            tickSystemSound

        liftIO (putStrLn "Entities:")
        saveEntities

        liftIO (putStrLn "Entities after remove:")
        removeEntity entity1
        
        saveEntities



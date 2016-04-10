{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module System.Physics where
import Control.Monad.State
import Data.ECS
import Data.Yaml
import GHC.Generics
import Control.Lens.Extra
import Physics.Bullet
import Linear.Extra
import Control.Monad.Reader
data PhysicsSystem = PhysicsSystem { _phyDynamicsWorld :: DynamicsWorld } deriving Show
makeLenses ''PhysicsSystem
defineSystemKey ''PhysicsSystem

data ShapeType = CubeShape | SphereShape deriving (Show, Generic, ToJSON, FromJSON)
defineComponentKey ''ShapeType

newtype Mass = Mass { unMass :: Float } deriving (Show, Generic, ToJSON, FromJSON)
defineComponentKey ''Mass

newtype Restitution = Restitution { unRestitution :: Float } deriving (Show, Generic, ToJSON, FromJSON)
defineComponentKey ''Restitution

defineComponentKey ''RigidBody

initSystemPhysics :: (MonadIO m, MonadState ECS m) => m ()
initSystemPhysics = do
    dynamicsWorld <- createDynamicsWorld mempty
    registerSystem sysPhysics (PhysicsSystem dynamicsWorld)

    registerComponent "RigidBody" myRigidBody $ (newComponentInterface myRigidBody) 
        { ciDeriveComponent = Just $ do
            let bodyInfo = mempty
            mShapeType <- getComponent myShapeType
            forM_ mShapeType $ \shapeType -> do
                shape     <- case shapeType of 
                    CubeShape -> createBoxShape (1 :: V3 Float)
                    SphereShape -> createSphereShape (1 :: Float)
                entityID <- ask
                rigidBody <- addRigidBody dynamicsWorld (CollisionObjectID entityID) shape bodyInfo
                setComponent myRigidBody rigidBody
        , ciRemoveComponent  = 
                withComponent_ myRigidBody $ \rigidBody -> do
                    removeRigidBody dynamicsWorld rigidBody
                    removeComponent myRigidBody
        }
    registerComponent "Mass"        myMass        (savedComponentInterface myMass)
    registerComponent "Restitution" myRestitution (savedComponentInterface myRestitution)
    registerComponent "ShapeType"   myShapeType   (savedComponentInterface myShapeType)

tickSystemPhysics :: (MonadState ECS m, MonadIO m) => m ()
tickSystemPhysics = do
    dynamicsWorld <- viewSystem sysPhysics phyDynamicsWorld
    stepSimulationSimple dynamicsWorld (1/60)

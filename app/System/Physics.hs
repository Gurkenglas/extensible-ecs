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

    registerComponent "RigidBody" cmpRigidBody $ (newComponentInterface cmpRigidBody) 
        { ciDeriveComponent = Just $ do
            let bodyInfo = mempty
            mShapeType <- getComponent cmpShapeType
            forM_ mShapeType $ \shapeType -> do
                shape     <- case shapeType of 
                    CubeShape -> createBoxShape (1 :: V3 Float)
                    SphereShape -> createSphereShape (1 :: Float)
                entityID <- ask
                rigidBody <- addRigidBody dynamicsWorld (CollisionObjectID entityID) shape bodyInfo
                setComponent cmpRigidBody rigidBody
        , ciRemoveComponent  = 
                withComponent_ cmpRigidBody $ \rigidBody -> do
                    removeRigidBody dynamicsWorld rigidBody
                    removeComponent cmpRigidBody
        }
    registerComponent "Mass"        cmpMass        (savedComponentInterface cmpMass)
    registerComponent "Restitution" cmpRestitution (savedComponentInterface cmpRestitution)
    registerComponent "ShapeType"   cmpShapeType   (savedComponentInterface cmpShapeType)

tickSystemPhysics :: (MonadState ECS m, MonadIO m) => m ()
tickSystemPhysics = do
    dynamicsWorld <- viewSystem sysPhysics phyDynamicsWorld
    stepSimulationSimple dynamicsWorld (1/60)

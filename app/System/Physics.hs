{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module System.Physics where
import Control.Monad.State
import Data.ECS
import Data.Yaml
import GHC.Generics
import Control.Lens
import Physics.Bullet
import Linear.Extra

data PhysicsSystem = PhysicsSystem { _phyDynamicsWorld :: DynamicsWorld } deriving Show
makeLenses ''PhysicsSystem
defineSystemKey ''PhysicsSystem

newtype Mass = Mass { unMass :: Float } deriving (Show, Generic, ToJSON, FromJSON)
defineComponentKey ''Mass

newtype Restitution = Restitution { unRestitution :: Float } deriving (Show, Generic, ToJSON, FromJSON)
defineComponentKey ''Restitution

defineComponentKey ''RigidBody

initSystemPhysics :: (MonadIO m, MonadState ECS m) => m ()
initSystemPhysics = do
    dynamicsWorld <- createDynamicsWorld mempty
    registerSystem sysPhysics (PhysicsSystem dynamicsWorld)

    registerComponent "RigidBody" cmpRigidBody $ ComponentInterface 
        { ciAddComponent     = \entityID -> do
                let bodyInfo = mempty
                shape <- createBoxShape (1 :: V3 Float)
                rigidBody <- addRigidBody dynamicsWorld (CollisionObjectID entityID) shape bodyInfo
                addComponent cmpRigidBody rigidBody entityID
        , ciExtractComponent = Nothing
        , ciRemoveComponent  = \entityID -> 
                withComponent entityID cmpRigidBody $ \rigidBody -> do
                    removeRigidBody dynamicsWorld rigidBody
                    removeComponent cmpRigidBody entityID
        }
    registerComponentSimple "Mass"        cmpMass        (Mass 2)
    registerComponentSimple "Restitution" cmpRestitution (Restitution 20)

tickSystemPhysics :: (MonadState ECS m, MonadIO m) => m ()
tickSystemPhysics = do
    dynamicsWorld <- viewSystem sysPhysics phyDynamicsWorld
    stepSimulation dynamicsWorld 90

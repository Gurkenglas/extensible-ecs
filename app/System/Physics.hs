{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module System.Physics where
import Control.Monad.State
import Rumpus
import Data.Yaml
import GHC.Generics
import Control.Lens
import Physics.Bullet
import Linear.Extra

data PhysicsSystem = PhysicsSystem { _psDynamicsWorld :: DynamicsWorld } deriving Show
makeLenses ''PhysicsSystem
defineSystemKey ''PhysicsSystem

newtype Mass = Mass { _cmpMass :: Float } deriving (Show, Generic, ToJSON, FromJSON)
defineComponentKey ''Mass

newtype Restitution = Restitution { _cmpRestitution :: Float } deriving (Show, Generic, ToJSON, FromJSON)
defineComponentKey ''Restitution

defineComponentKey ''RigidBody

initSystemPhysics :: (MonadIO m, HasECS s, MonadState s m) => m ()
initSystemPhysics = do
    dynamicsWorld <- createDynamicsWorld mempty
    registerSystem physicsSystemKey (PhysicsSystem dynamicsWorld)

    registerComponent "RigidBody" rigidBodyKey $ ComponentInterface 
        { ciAddComponent     = \entityID -> do
                let bodyInfo = mempty
                shape <- createBoxShape (1 :: V3 Float)
                rigidBody <- addRigidBody dynamicsWorld (CollisionObjectID entityID) shape bodyInfo
                addComponentToEntity rigidBodyKey rigidBody entityID
        , ciExtractComponent = Nothing
        , ciRemoveComponent  = \entityID -> 
                withComponent entityID rigidBodyKey $ \rigidBody -> do
                    removeRigidBody dynamicsWorld rigidBody
                    removeComponentFromEntity rigidBodyKey entityID
        }
    registerComponentSimple "Mass"        massKey        (Mass 2)
    registerComponentSimple "Restitution" restitutionKey (Restitution 20)

tickSystemPhysics :: (HasECS s, MonadState s m, MonadIO m) => m ()
tickSystemPhysics = do
    withSystem physicsSystemKey $ \(PhysicsSystem dynamicsWorld) -> stepSimulation dynamicsWorld 90
    return ()



{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE BangPatterns #-}
module Data.ECS.Vault where

import Data.Dynamic
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import GHC.Exts
import Unsafe.Coerce
--import Data.Hashable
--import Control.Lens.Extra

-- | An embedding of the 'vault' package with the Key type based simply on Ints,
-- such that we can create them at compile-time based on a hash of a given name.
-- (See Data.ECS.TH.defineKey)

newtype Vault = Vault (HashMap Int Dynamic) 
    --deriving (Eq, Monoid, Ixed, At)
    deriving (Monoid)

newtype Key a = Key Int 
    deriving (Show)

--type role Vault nominal 
type role Key nominal 

lookup :: Key a -> Vault -> Maybe a
lookup (Key key) (Vault vals) = fromDynamic =<< Map.lookup key vals

insert :: Key a -> a -> Vault -> Vault
insert (Key key) !value (Vault vals) = Vault $ Map.insert key (toDyn value) vals 

adjust :: (a -> a) -> Key a -> Vault -> Vault
adjust f (Key k) (Vault m) = Vault $ Map.adjust f' k m 
     where f' = toAny . f . fromAny 

delete :: Key a -> Vault -> Vault
delete (Key k) (Vault m) = Vault $ Map.delete k m

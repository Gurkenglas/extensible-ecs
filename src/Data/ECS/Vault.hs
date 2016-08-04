{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.ECS.Vault where

import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import GHC.Exts
import Unsafe.Coerce
import Data.Dynamic
--import Data.Hashable
--import Control.Lens.Extra

-- | An embedding of the 'vault' package with the Key type based simply on Ints,
-- such that we can create them at compile-time based on a hash of a given name.
-- (See Data.ECS.TH.defineKey)

newtype Vault = Vault (HashMap Int Any) 
    --deriving (Eq, Monoid, Ixed, At)
    deriving (Monoid)

makeWrapped ''Vault

newtype Key a = Key Int 
    deriving (Show)

--type role Vault nominal 
type role Key nominal 

vault :: Key a -> Lens' Vault (Maybe a)
vault (Key key) = _Wrapping Vault . at key . iso (>>= fromDynamic) (fmap toDyn)

{-# LANGUAGE TemplateHaskell #-}
module Data.ECS.TH where
import Language.Haskell.TH
import System.IO.Unsafe
import           Data.Vault.Strict (Key)
import qualified Data.Vault.Strict as Vault
import Data.ECS.Types
import Data.List

{- |
Generates definitions of the form:
{-# NOINLINE myKey #-}
myKey :: Key MyType
myKey = unsafePerformIO newKey
-}
defineKey :: String -> TypeQ -> DecsQ
defineKey keyString keyType = sequence [inlineDecl, signatureDecl, keyDecl]
    where
        keyName       = mkName keyString
        inlineDecl    = pragInlD keyName NoInline FunLike AllPhases
        signatureDecl = sigD keyName (conT ''Key `appT` keyType)
        keyDecl       = valD (varP keyName) (normalB (appE (varE 'unsafePerformIO) (varE 'Vault.newKey))) []

{- |
defineSystemKey ''PhysicsSystem
will create a key definition of the form:
sysPhysics :: Key PhysicsSystem
-}
defineSystemKey :: Name -> DecsQ
defineSystemKey name = do
    let systemKeyName = "sys" ++ deleteWord "System" (nameBase name)
    defineKey systemKeyName (conT name)


{- |
defineComponentKey ''Color
will create a key defintion of the form:
cmpColor :: Key (EntityMap Color)
cmp1Color :: Key Color
-}
defineComponentKey :: Name -> DecsQ
defineComponentKey name = defineComponentKeyWithType (nameBase name) (conT name)

defineComponentKeyWithType :: String -> TypeQ -> DecsQ
defineComponentKeyWithType name keyType = defineKey ("cmp" ++ name)  (conT ''EntityMap `appT` keyType)
    -- (++) 
        -- <$> defineKey ("cmp" ++ name)  (conT ''EntityMap `appT` keyType)
        -- <*> defineKey ("cmp1" ++ name) keyType

{- | 
>>> deleteWord "PhysicsSystem"  
"Physics"
-}
deleteWord :: String -> String -> String
deleteWord _    [] = []
deleteWord word (x:xs)
    | word `isPrefixOf` (x:xs) = drop (length word) (x:xs)
    | otherwise                = (x:deleteWord word xs)

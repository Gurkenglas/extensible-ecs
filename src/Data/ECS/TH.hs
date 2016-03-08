{-# LANGUAGE TemplateHaskell #-}
module Data.ECS.TH where
import Language.Haskell.TH
import System.IO.Unsafe
--import           Data.Vault.Strict (Key)
--import qualified Data.Vault.Strict as Vault
import Data.ECS.Types
import Data.ECS.Vault
import Data.List
import Data.Hashable

{- |
Generates definitions of the form:
myKey :: Key MyType
myKey = Key 1283671237472

where 1283671237472 is a hash of the string 'myKey'
-}
defineKey :: String -> TypeQ -> DecsQ
defineKey keyString keyType = sequence [signatureDecl, keyDecl]
    where
        keyName       = mkName keyString
        signatureDecl = sigD keyName (conT ''Key `appT` keyType)
        keyIntLit     = IntegerL $ fromIntegral $ hash keyString
        keyDecl       = valD (varP keyName) (normalB (conE 'Key `appE` litE keyIntLit)) []

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
>>> deleteWord "PhysicsSystem" "System"
"Physics"
-}
deleteWord :: String -> String -> String
deleteWord _    [] = []
deleteWord word (x:xs)
    | word `isPrefixOf` (x:xs) = drop (length word) (x:xs)
    | otherwise                = (x:deleteWord word xs)

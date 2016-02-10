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
{-# NOINLINE soundSystemKey #-}
soundSystemKey :: Key SoundSystem
soundSystemKey = unsafePerformIO newKey
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
physicsSystemKey :: Key PhysicsSystem
-}
defineSystemKey :: Name -> DecsQ
defineSystemKey name = do
    let systemKeyName = "sys" ++ deleteWord "System" (nameBase name)
    defineKey systemKeyName (conT name)


{- |
defineComponentKey ''ColorComponent
will create a key defintion of the form:
colorComponentKey :: Key (EntityMap ColorComponent)
-}
defineComponentKey :: Name -> DecsQ
defineComponentKey name = do
    let componentKeyName = "cmp" ++ (nameBase name)
    defineKey componentKeyName (conT ''EntityMap `appT` conT name)

{- | 
>>> deleteWord "PhysicsSystem"  
"Physics"
-}
deleteWord :: String -> String -> String
deleteWord _    [] = []
deleteWord word (x:xs)
    | word `isPrefixOf` (x:xs) = drop (length word) (x:xs)
    | otherwise                = (x:deleteWord word xs)

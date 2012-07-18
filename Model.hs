{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
module Model where

import Prelude
import Yesod
import Database.Persist.Quasi
import Database.Persist.Store
import Data.Serialize
import qualified Data.Text as T
import GHC.Generics

import EveApi.Types (
        Optional (..), APIArgumentType (..), APIDataType (..)
    ,   Scope (..), Call (..), KeyType (..), Arg (..), CallArgs (..))

deriving instance Generic Optional
deriving instance Generic APIArgumentType
deriving instance Generic APIDataType
deriving instance Generic Call
deriving instance Generic Scope
deriving instance Generic KeyType
deriving instance Generic Arg

instance Serialize Optional
instance Serialize APIArgumentType
instance Serialize APIDataType
instance Serialize Call
instance Serialize Scope
instance Serialize KeyType
instance Serialize Arg

-- being lazy and just serializing things via GHC.Generic
instance PersistField Call where
    toPersistValue c = PersistByteString (encode c)
    fromPersistValue (PersistByteString s) = either (Left . T.pack) Right (decode s)
    fromPersistValue _                     = Left "internal error: invalid Call"
    sqlType _ = SqlBlob
    isNullable _ = False
instance PersistField Scope where
    toPersistValue s = PersistByteString (encode s)
    fromPersistValue (PersistByteString s) = either (Left . T.pack) Right (decode s)
    fromPersistValue _                     = Left "internal error: invalid Scope"
    sqlType _ = SqlBlob
    isNullable _ = False
instance PersistField KeyType where
    toPersistValue k = PersistByteString (encode k)
    fromPersistValue (PersistByteString s) = either (Left . T.pack) Right (decode s)
    fromPersistValue _                     = Left "internal error: invalid KeyType"
    sqlType _ = SqlBlob
    isNullable _ = False
instance PersistField Arg where
    toPersistValue a = PersistByteString (encode a)
    fromPersistValue (PersistByteString s) = either (Left . T.pack) Right (decode s)
    fromPersistValue _                     = Left "internal error: invalid Arg"
    sqlType _ = SqlBlob
    isNullable _ = False

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

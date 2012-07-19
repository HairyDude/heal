{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Orphans () where

-- | This module contains additional instances which must be orphans. This is
-- so we can turn off warnings about them in only one module and leave them on
-- elsewhere.

import Prelude (($), show, Maybe (..))

import qualified Data.Text as T

import Yesod
    (   PersistField (..)
    ,   PathPiece (..)
    ,   derivePersistField
    )

import EveApi.Types
    (   Call (CallList)
    ,   Scope, scopeString, stringScope
    ,   KeyType
    ,   Arg
    )
import Utils (maybeRead)

derivePersistField "Call"
derivePersistField "Scope"
derivePersistField "KeyType"
derivePersistField "Arg"

instance PathPiece Call where
    toPathPiece CallList = "calllist"
    toPathPiece c        = T.pack $ show c
    fromPathPiece "calllist" = Just CallList
    fromPathPiece s          = maybeRead (T.unpack s)
instance PathPiece Scope where
    toPathPiece   = scopeString
    fromPathPiece = stringScope

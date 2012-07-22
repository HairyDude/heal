{-# OPTIONS_GHC -fno-warn-orphans #-}
module EveApi.Orphans () where

-- | This module contains additional instances which must be orphans. This is
-- so we can turn off warnings about them in only one module and leave them on
-- elsewhere.

import Prelude ((.))

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (ExceptionT)

instance MonadIO m => MonadIO (ExceptionT m) where
    liftIO = lift . liftIO


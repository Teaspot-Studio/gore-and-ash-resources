{-|
Module      : Game.GoreAndAsh.Resources.API
Description : Monadic and arrow API for module
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Game.GoreAndAsh.Resources.API(
    MonadResources(..)
  ) where

import Control.Monad.Trans 
import Control.Monad.State.Strict
import qualified Data.HashMap.Strict as H

import Game.GoreAndAsh.Resources.Module
import Game.GoreAndAsh.Resources.State


-- | Low level monadic API for module.
--
-- Note: does not require 'm' to be 'IO' monad.
class Monad m => MonadResources m where 
  -- | stab method, temporary
  resourcesIsCached :: ResPath -> m Bool
  resourcesToCache :: (Resourcable a) => a -> m ()
  resourcesFromCache :: (Resourcable a) => ResPath -> m (Either String a)

instance {-# OVERLAPPING #-} Monad m => MonadResources (ResourcesT s m) where
  resourcesIsCached rp = do
    s <- get
    let resMap = resourcesCache s
    return $ H.member rp resMap
  resourcesToCache _ = return ()
  resourcesFromCache _ = return $ Left "undefined"

instance {-# OVERLAPPABLE #-} (Monad (mt m), MonadResources m, MonadTrans mt) => MonadResources (mt m) where 
  resourcesIsCached rp = lift $ resourcesIsCached rp
  resourcesToCache a = lift $ resourcesToCache a
  resourcesFromCache rp = lift $ resourcesFromCache rp
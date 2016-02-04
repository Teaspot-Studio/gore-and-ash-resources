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

import Game.GoreAndAsh.Resources.Module 

-- | Low level monadic API for module.
--
-- Note: does not require 'm' to be 'IO' monad.
class Monad m => MonadResources m where 
  -- | stab method, temporary
  resourcesStab :: m ()

instance {-# OVERLAPPING #-} Monad m => MonadResources (ResourcesT s m) where
  resourcesStab = return ()

instance {-# OVERLAPPABLE #-} (Monad (mt m), MonadResources m, MonadTrans mt) => MonadResources (mt m) where 
  resourcesStab = lift resourcesStab
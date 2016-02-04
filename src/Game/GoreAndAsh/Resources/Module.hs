{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Game.GoreAndAsh.Resources.Module
Description : Monad transformer and instance for core module
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Game.GoreAndAsh.Resources.Module(
    ResourcesT(..)
  ) where

import Control.Monad.Catch
import Control.Monad.Fix 
import Control.Monad.State.Strict

import Game.GoreAndAsh
import Game.GoreAndAsh.Resources.State

-- | Monad transformer of the core module.
--
-- [@s@] - State of next core module in modules chain;
--
-- [@m@] - Next monad in modules monad stack;
--
-- [@a@] - Type of result value;
--
-- How to embed module:
-- 
-- @
-- type AppStack = ModuleStack [ResourcesT, ... other modules ... ] IO
--
-- newtype AppMonad a = AppMonad (AppStack a)
--   deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadThrow, MonadCatch, MonadResources)
-- @
--
-- The module is pure within first phase (see 'ModuleStack' docs), therefore 'Identity' can be used as end monad.
newtype ResourcesT s m a = ResourcesT { runResourcesT :: StateT (ResourcesState s) m a }
  deriving (Functor, Applicative, Monad, MonadState (ResourcesState s), MonadFix, MonadTrans, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance GameModule m s => GameModule (ResourcesT s m) (ResourcesState s) where 
  type ModuleState (ResourcesT s m) = ResourcesState s
  runModule (ResourcesT m) s = do
    ((a, s'), nextState) <- runModule (runStateT m s) (resourcesNextState s)
    return (a, s' {
        resourcesNextState = nextState 
      })  
  
  newModuleState = emptyResourcesState <$> newModuleState
  withModule _ = id
  cleanupModule _ = return ()

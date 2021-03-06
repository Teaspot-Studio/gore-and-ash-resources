{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-|
Module      : Game.GoreAndAsh.Resources
Description : Core module for embedding concurrent IO actions into main loop.
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The core module contains API for something into main game loop for Gore&Ash.

The module does not depend on any other core modules, so 'ResourcesT' could be placed at any place in monad stack.

The module is pure within first phase (see 'ModuleStack' docs), therefore 'Identity' can be used as end monad.

Example of embedding:

@
-- | Application monad is monad stack build from given list of modules over base monad (IO or Identity)
type AppStack = ModuleStack [ResourcesT ... other modules ... ] IO
newtype AppState = AppState (ModuleState AppStack)
  deriving (Generic)

instance NFData AppState 

-- | Wrapper around type family
newtype AppMonad a = AppMonad (AppStack a)
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadThrow, MonadCatch, MonadResources ... other modules monads ... )

instance GameModule AppMonad AppState where 
  type ModuleState AppMonad = AppState
  runModule (AppMonad m) (AppState s) = do 
    (a, s') <- runModule m s 
    return (a, AppState s')
  newModuleState = AppState <$> newModuleState
  withModule _ = withModule (Proxy :: Proxy AppStack)
  cleanupModule (AppState s) = cleanupModule s 

-- | Arrow that is build over the monad stack
type AppWire a b = GameWire AppMonad a b
@

-}
module Game.GoreAndAsh.Resources(
  -- * Low-level
    ResourcesState
  , ResourcesT 
  , MonadResources(..)
  ) where

-- for docs
import Game.GoreAndAsh

import Game.GoreAndAsh.Resources.API as X 
import Game.GoreAndAsh.Resources.Module as X 
import Game.GoreAndAsh.Resources.State as X 

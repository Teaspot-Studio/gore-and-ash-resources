{-|
Module      : Game.GoreAndAsh.Resources.State
Description : Internal state of core module
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Game.GoreAndAsh.Resources.State(
    ResourcesState(..)
  , emptyResourcesState
  ) where

import Control.DeepSeq
import GHC.Generics (Generic)

-- | Internal state of core module
--
-- [@s@] - state of next module, they are chained until bottom, that is usually
--         an empty data type.
data ResourcesState s = ResourcesState {
  -- | Next module state in chain of modules
  resourcesNextState :: !s
} deriving (Generic)

instance NFData s => NFData (ResourcesState s)

-- | Create inital state of the core module
--
-- [@s@] -  state of next module
emptyResourcesState :: s -> ResourcesState s 
emptyResourcesState s = ResourcesState {
    resourcesNextState = s
  }

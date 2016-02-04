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
  , ResPath
  , Resourcable
  , emptyResourcesState
  ) where

import Control.DeepSeq
import GHC.Generics (Generic)
import Data.Dynamic
import Data.Text
import Data.Typeable
import qualified Data.HashMap.Strict as H

type ResPath = Text

class Typeable a => Resourcable a where
  resFoo :: a -> Bool

-- | Internal state of core module
--
-- [@s@] - state of next module, they are chained until bottom, that is usually
--         an empty data type.
data ResourcesState s = ResourcesState {
  -- | Next module state in chain of modules
  resourcesCache :: H.HashMap ResPath Dynamic
, resourcesNextState :: !s
} deriving (Generic)

instance NFData s => NFData (ResourcesState s) where
  rnf ResourcesState {..} = 
    resourcesCache `seq`
    resourcesNextState `deepseq` ()

-- | Create inital state of the core module
--
-- [@s@] -  state of next module
emptyResourcesState :: s -> ResourcesState s 
emptyResourcesState s = ResourcesState {
    resourcesCache = H.empty
  , resourcesNextState = s
  }

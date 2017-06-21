module HashLife.Population
  ( Population(..)
  ) where

import qualified HashLife.Atomic    as Atomic
import qualified HashLife.Composite as Composite

data Population = Atomic Atomic.Population
                | Composite { comp :: Composite.Population
                            , sub :: Population
                            }

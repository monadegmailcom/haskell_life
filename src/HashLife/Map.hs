module HashLife.Map
  ( Map
  , Reference
  ) where

import qualified Data.HashMap.Strict as HM

type Reference = Int

type Map = HM.HashMap Reference

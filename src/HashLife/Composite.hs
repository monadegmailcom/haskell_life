module HashLife.Composite
  ( QuadTree
  , Population
  ) where

import           HashLife.Map        ( Map, Reference )
import qualified HashLife.QuadTree   as QT

type QuadTree = QT.QuadTree Reference

type Population = Map QuadTree

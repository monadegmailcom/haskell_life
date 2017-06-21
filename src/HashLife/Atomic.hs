module HashLife.Atomic
  ( Atomic
  , Population
  , QuadTree
  ) where

import           Data.Word           ( Word16 )

import           HashLife.Map        ( Map )
import qualified HashLife.QuadTree   as QT

type Atomic = Word16

type QuadTree = QT.QuadTree Atomic

type Population = Map QuadTree


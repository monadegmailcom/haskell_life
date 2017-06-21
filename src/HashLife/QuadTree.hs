module HashLife.QuadTree
  ( QuadTree(..)
  ) where

data QuadTree a = QuadTree
                { nw :: a
                , ne :: a
                , sw :: a
                , se :: a
                }


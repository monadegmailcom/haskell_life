module Grid2dModel (
   Command (..),
   Update (..),
   run
   ) where

import           Control.Concurrent.MVar
import qualified Data.Set                as Set

import qualified Conway

data Command =
    Exit |
    ToggleCell (Int,Int) |
    SingleStep |
    Run |
    Clear

data Update = UpdateCells [(Int,Int)]

-- run thread with view2Model and model2View mvar
run :: MVar Command -> MVar Update -> IO ()
run = implRun Set.empty

runWorld :: Conway.World -> MVar Command -> MVar Update -> IO ()
runWorld w v2m m2v = do
  let w' = Conway.nextTick w
  let cs = Conway.getCells w'
  putMVar m2v $ UpdateCells cs
  maybeCmd <- tryTakeMVar v2m
  case maybeCmd of
    Nothing -> runWorld w' v2m m2v
    _          -> implRun (Set.fromList cs) v2m m2v

implRun :: Set.Set (Int, Int) -> MVar Command -> MVar Update -> IO ()
implRun cs view2model model2view = do
  cmd <- takeMVar view2model
  case cmd of
    Exit -> return ()
    ToggleCell c -> do
      let cs' = f c cs
      putMVar model2view $ UpdateCells (Set.elems cs')
      implRun cs' view2model model2view
    SingleStep -> do
      let w = Conway.initialize Conway.LazyMap $ Set.toList cs
      let w' = Conway.nextTick w
      let cs = Conway.getCells w'
      putMVar model2view $ UpdateCells cs
      implRun (Set.fromList cs) view2model model2view
    Run -> do
      let w = Conway.initialize Conway.LazyMap $ Set.toList cs
      runWorld w view2model model2view
    Clear -> do
      putMVar model2view $ UpdateCells []
      implRun Set.empty view2model model2view
  where
    -- toggle cell membership in set
    f c set
      | Set.member c set = Set.delete c set
      | otherwise        = Set.insert c set

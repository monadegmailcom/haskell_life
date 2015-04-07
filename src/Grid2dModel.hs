module Grid2dModel (
   Command (..),
   Update (..),
   run
   ) where

import           Control.Concurrent.MVar
import qualified Conway

data Command =
    Exit |
    ToggleCell (Int,Int) |
    SingleStep |
    Run |
    Clear

data Update = UpdateCells [(Int,Int)]

-- create new world
newWorld :: Conway.World
newWorld = Conway.initialize Conway.LazyMap []

-- run thread with view2Model and model2View mvar
run :: MVar Command -> MVar Update -> IO ()
run = implRun newWorld

runWorld :: Conway.World -> MVar Command -> MVar Update -> IO ()
runWorld w v2m m2v = do
  let w' = Conway.nextTick w
  putMVar m2v $ UpdateCells (Conway.getCells w')
  maybeCmd <- tryTakeMVar v2m
  case maybeCmd of
    Nothing -> runWorld w' v2m m2v
    _       -> implRun w' v2m m2v

implRun :: Conway.World -> MVar Command -> MVar Update -> IO ()
implRun w view2model model2view = do
  cmd <- takeMVar view2model
  case cmd of
    Exit -> return ()
    ToggleCell c -> do
      let w' = Conway.modifyCell w c not
      putMVar model2view $ UpdateCells (Conway.getCells w')
      implRun w' view2model model2view
    SingleStep -> do
      let w' = Conway.nextTick w
      putMVar model2view $ UpdateCells (Conway.getCells w')
      implRun w' view2model model2view
    Run -> do
      runWorld w view2model model2view
    Clear -> do
      putMVar model2view $ UpdateCells []
      implRun newWorld view2model model2view

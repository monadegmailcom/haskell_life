module Grid2dModel (
   Command (..),
   Update (..),
   run
   ) where

import Control.Concurrent.MVar
import qualified Data.Set as Set 

data Command =
    Exit |
    ToggleCell (Int,Int) 

data Update =
    UpdateCells [(Int,Int)]

-- run thread with view2Model and model2View mvar 
run :: MVar Command -> MVar Update -> IO ()
run = implRun Set.empty 

implRun :: Set.Set (Int, Int) -> MVar Command -> MVar Update -> IO ()
implRun cs view2model model2view = do
  cmd <- takeMVar view2model
  case cmd of
    Exit -> return ()
    ToggleCell c -> do
      let cs' = f c cs
      putMVar model2view $ UpdateCells (Set.elems cs')
      implRun cs' view2model model2view
  where 
    -- toggle cell membership in set
    f c set   
      | Set.member c set = Set.delete c set  
      | otherwise        = Set.insert c set 



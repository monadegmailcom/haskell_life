-- | Main entry point to the application.
module Main where

import qualified Data.Map as Map
import Data.Maybe

-- cell type
type Cell = Bool

-- empty cell
emptyCell :: Cell
emptyCell = False

-- coordinate type
type Coord = (Int, Int)

-- topology function (conway's game of life)
getNbh :: Coord -> [Coord]
getNbh (x,y) = [(x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y), (x+1,y), (x-1,y+1), (x,y+1), (x+1,y+1)]

-- evolution function
evolve :: Coord -> Cell -> [(Coord, Cell)] -> Cell
evolve _ cell nbh
    | not cell  = c == 3 -- birth when 3 neighbours
    | otherwise = c >= 2 && c <= 4 -- survival when between 2 and 4 neighbours
    where 
          c :: Int
          c = sum' nbh -- count neighbours
          sum' = foldr f 0
          f (_,True) s = s+1
          f _        s = s

-- evolve cells and candidates one tick
-- input: map of coordinate to pair of candidate flag and cell 
-- the candidates flag indicates if a cell is checked for evolution in next tick
-- non candidates are just copied
-- a candidate is a cell which have changed or for which at least one neighbour has changed
-- output: next generation map
-- promise: empty cells are candidates
nextTick :: Map.Map Coord (Bool, Cell) -> Map.Map Coord (Bool, Cell)
-- fold previous map in new map starting with empty map
nextTick cells = Map.foldrWithKey f Map.empty cells where
    f coord (cand, cell) accCells = accCells' where
        accCells'
            -- just copy non-candidates, flag as candidate if already in accumulated map
            | not cand = Map.alter g coord accCells
            -- insert evolved cell and all neighbours in accumulated map
            | otherwise = foldr h accCells $ (coord, newCell):nbs
        -- flag as candidate if in accumulated map
        g x = Just (isJust x, cell) 
        -- insert as candidate cell if cell changed
        h (x,y)  = Map.insert x (cell /= newCell, y)
        -- new cell evolves from previous cell
        newCell = evolve coord cell nbs
        -- build neighbourhood list as (coord, cell) pairs
        nbs = map i $ getNbh coord
        -- find cell from coord
        i coord'  = (coord', snd (Map.findWithDefault (False, emptyCell) coord' cells))

-- initialize cell map from cell list
initialize :: [(Coord, Cell)] -> Map.Map Coord (Bool, Cell)
-- accumulate map from all cells and their neighbours starting with empty map
initialize cells = foldr f Map.empty cells' where
    -- insert candidate cell when not in already
    f (coord, cell) = Map.alter g coord where 
        g Nothing = Just (True, cell)
        g _ = Nothing
    -- add neighbours to cell list
    cells' = cells ++ zip nbs (repeat emptyCell)
    nbs = concatMap getNbh $ fst $ unzip cells
    
-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Game of life"
    let cells = [((0,0), True)]
    print $ initialize cells

-- | Main entry point to the application.
module Main where

import qualified Data.Map as Map
import Data.Maybe
import Control.Monad

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
    | otherwise = c >= 2 && c <= 3 -- survival when between 2 and 3 neighbours
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
            | not changed = cells2
            | otherwise  = foldr h cells2 nbs
        -- flag as candidate if in accumulated map
        g x = Just (isJust x, cell)
        cells2 
            | newCell == emptyCell = accCells
            | otherwise = Map.alter s coord accCells where
                s Nothing = Just (changed, newCell)
                s _       = Just (True, newCell)
        -- insert as candidate cell if cell changed
        h (coord2, _) = Map.alter j coord2
        j Nothing = Just (True, emptyCell)
        j (Just (_, c))    = Just (True, c)
        -- new cell evolves from previous cell
        newCell = evolve coord cell nbs
        -- build neighbourhood list as (coord, cell) pairs
        nbs = map i $ getNbh coord
        -- find cell from coord
        i coord'  = (coord', snd (Map.findWithDefault (False, emptyCell) coord' cells))
        changed = cell /= newCell
        
-- initialize cell map from cell list
initialize :: [Coord] -> Map.Map Coord (Bool, Cell)
-- accumulate map from all cells and their neighbours starting with empty map
initialize coords = foldr f cells nbs where
    -- insert candidate cell when not in already
    f = Map.alter g where
        g Nothing = Just (True, False)
        g x       = x
    -- add neighbours to cell list
    nbs = concatMap getNbh coords
    cellList = zip coords $ repeat (True, True)
    cells = Map.fromList cellList
    
-- get enclosing coordinate rectangle frame of all cells (minX, minY, maxX, maxY)
getFrame :: Map.Map Coord (Bool, Cell) -> (Int, Int, Int, Int)
getFrame cells 
    | cells == Map.empty = (0, 0, 0, 0)
    | otherwise          = (minX, minY, maxX, maxY) where
    minX = minimum $ fst $ unzip $ Map.keys cells
    minY = minimum $ snd $ unzip $ Map.keys cells
    maxX = maximum $ fst $ unzip $ Map.keys cells
    maxY = maximum $ snd $ unzip $ Map.keys cells

-- draw cells
draw :: Map.Map Coord (Bool, Cell) -> String
draw cells = header ++ pic where
    pic = snd $ until p g ((maxX, maxY), "\n")
    p (pos, _) = pos == (maxX, minY-1)
    g (pos@(posX, posY), acc)
        | posX == minX = ((maxX, posY-1), '\n':acc2)
        | otherwise    = ((posX-1, posY), acc2) where
        acc2 = h mCell : acc
        mCell = Map.lookup pos cells 
    h (Just (cand, cell))
        | cell == emptyCell             = '.'
        | cand                          = 'x'
        | otherwise                     = 'X'
    h Nothing = ' '
    header = show frame ++ "\n"
    frame@(minX, minY, maxX, maxY) = getFrame cells 

-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Game of life"
    let cells = initialize [(0,2),(1,2),(2,2),(2,1),(1,0)] 
    f cells where
        f :: Map.Map Coord (Bool, Cell) -> IO ()
        f cells = do
            putStr $ draw cells
            input <- getChar
            unless (input == 'q') $ f $ nextTick cells

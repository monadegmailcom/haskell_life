-- | Main entry point to the application.
module Main where

import qualified Data.Map as Map
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
    -- birth when 3 neighbours
    | not cell  = c == 3 
    -- survival when between 2 and 3 neighbours
    | otherwise = c >= 2 && c <= 3 where 
        -- count neighbours
        c :: Int
        c = let f (_,True) s = s+1
                f _        s = s
            in foldr f 0 nbh 

-- evolve cells and candidates one tick
-- input: map of coordinate to pair of candidate flag and cell 
-- the candidates flag indicates if a cell is checked for evolution in next tick
-- non candidates are not re-evaluated
-- a candidate is a cell for which at least one neighbour has changed
-- output: next generation map
-- promise: empty cells in map are candidates
nextTick :: Map.Map Coord (Bool, Cell) -> Map.Map Coord (Bool, Cell)
-- fold previous map in new map starting with empty map
nextTick cells = Map.foldrWithKey f Map.empty cells where
    f coord (cand, cell) acc
        -- unchanged and non-empty? see def
        | not changed && cell /= emptyCell = acc2
        -- changed? same as above plus all neighbours
        | changed  = foldr f2 acc2 nbs
        -- otherwise skip this cell
        | otherwise = acc where
            -- update accumulated cells
            acc2 = Map.alter f3 coord acc where
                f3 :: Maybe (Bool, Cell) -> Maybe (Bool, Cell)
                -- insert as non-candidate when not already inserted
                f3 Nothing = Just (False, newCell)
                -- replace cell when found
                f3 (Just (x, _)) = Just (x, newCell)
            -- insert as candidate cell if cell changed
            f2 (cand2, _) = Map.alter f3 cand2 where
                f3 Nothing       = Just (True, emptyCell)
                f3 (Just (_, x)) = Just (True, x)
            -- new cell evolves from previous cell
            (newCell, changed)
                | not cand  = (cell, False)
                | otherwise = (cell2, changed2) where
                    cell2    = evolve coord cell nbs
                    changed2 = cell2 /= cell
        -- build neighbourhood list as (coord, cell) pairs
            nbs = map f3 $ getNbh coord where
                -- find cell from coord
                f3 coord2  = (coord2, snd (Map.findWithDefault (False, emptyCell) coord2 cells))
        
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
        minX = minimum $ fst pair
        minY = minimum $ snd pair
        maxX = maximum $ fst pair
        maxY = maximum $ snd pair
        pair = unzip $ Map.keys cells
    
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
    --let floater = initialize [(0,2),(1,2),(2,2),(2,1),(1,0)] 
    --let block = initialize [(0,0),(0,1),(1,0),(1,1)]
    --let blinker = initialize [(0,0), (0,1),(0,2)]
    let rpentomino = initialize [(1,0),(2,0),(0,1),(1,1),(1,2)]
    f rpentomino 0 where
        f :: Map.Map Coord (Bool, Cell) -> Int -> IO ()
        f cells n = do
            print n
            putStr $ draw cells
            input <- getChar
            unless (input == 'q') $ f (nextTick cells) (n+1)

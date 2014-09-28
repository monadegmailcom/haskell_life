-- | Main entry point to the application.
module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set
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
nextTick :: Map.Map Coord Cell -> Set.Set Coord -> (Map.Map Coord Cell, Set.Set Coord)
nextTick cells coords = (remCells `Map.union` newCells, newCands) where
    -- iterate all previous candidates and accumulate next cells and candidates
    (remCells, newCells, newCands) = Set.foldr f (cells, Map.empty, Set.empty) coords where 
        f coord (accRemCells, accCells, accCands) = (accRemCells', accCells', accCands') where
            accRemCells' = Map.delete coord accRemCells
            accCells'
                | newCell == emptyCell = accCells
                | otherwise            = Map.insert coord newCell accCells
            accCands'
                | newCell == prevCell = accCands
                | otherwise          = foldr Set.insert accCands (coord:nbh)
            prevCell = fromMaybe emptyCell (Map.lookup coord cells)
            newCell = evolve coord prevCell nbh' where
                nbh' = map g nbh
                g coord' = (coord', Map.findWithDefault emptyCell coord' cells)
            nbh = getNbh coord

nextTick2 :: Map.Map Coord (Bool, Cell) -> Map.Map Coord (Bool, Cell)
nextTick2 cells = Map.foldrWithKey f M where
    cells' = Map.empty

-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Welcome to FP Haskell Center!"
    putStrLn "Have a good day!"
    print $ getNbh (0,0)

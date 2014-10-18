-- | Main entry point to the application.
module Main where

import qualified CGL
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
    --let rpentomino = initialize [(1,0),(2,0),(0,1),(1,1),(1,2)]

    f rpentomino 0 where
        nextTick = CGL.nextTick False Map.empty getNbh evolve Map.alter Map.findWithDefault Map.foldrWithKey
        rpentomino = CGL.initialize False getNbh Map.alter Map.fromList $ zip [(1,0),(2,0),(0,1),(1,1),(1,2)] (repeat True)
        
        f :: Map.Map Coord (Bool, Cell) -> Int -> IO ()
        f cells n = do
            print n
            putStr $ draw cells
            input <- getChar
            unless (input == 'q') $ f (nextTick cells) (n+1)

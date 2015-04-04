module Conway (
    initialize,
    nextTick,
    draw,
    Container(..),
    Coord,
    Cell,
    World,
    Statistic(..),
    getCells,
    getStatistic ) where

import qualified CGL
import qualified Data.Map.Strict as SMap
import qualified Data.Map.Lazy   as LMap

-- cell type
type Cell = Bool

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

data Container =
    StrictMap |
    LazyMap

data World =
    Strict (SMap.Map Coord (Bool, Cell)) |
    Lazy   (LMap.Map Coord (Bool, Cell))

nextTick :: World -> World
nextTick (Strict w) = Strict $ CGL.nextTick False SMap.empty getNbh evolve SMap.alter SMap.findWithDefault SMap.foldrWithKey w
nextTick (Lazy   w) = Lazy   $ CGL.nextTick False LMap.empty getNbh evolve LMap.alter LMap.findWithDefault LMap.foldrWithKey w

initialize :: Container -> [Coord] -> World
initialize StrictMap ls = Strict $ CGL.initialize False getNbh SMap.alter SMap.fromList $ zip ls (repeat True)
initialize LazyMap ls = Lazy $ CGL.initialize False getNbh LMap.alter LMap.fromList $ zip ls (repeat True)

-- get enclosing coordinate rectangle frame of all cells (minX, minY, maxX, maxY)
getFrame :: [Coord] -> (Int, Int, Int, Int)
getFrame [] = (0, 0, 0, 0)
getFrame coords = (minimum xs, minimum ys, maximum xs, maximum ys) where
    (xs, ys) = unzip coords

mylookup :: World -> Coord -> Maybe (Bool, Cell)
mylookup (Strict cells) coord = SMap.lookup coord cells
mylookup (Lazy   cells) coord = LMap.lookup coord cells

mykeys :: World -> [Coord]
mykeys (Strict cells) = SMap.keys cells
mykeys (Lazy   cells) = LMap.keys cells

getCells :: World -> [Coord]
getCells (Lazy cs) = map fst . filter p . LMap.toList $ cs where
  p (_, (_,c)) = c
getCells (Strict cs) = map fst . filter p . LMap.toList $ cs where
    p (_, (_,c)) = c

-- draw cells
draw :: World -> String
draw w = header ++ pic where
    pic = snd $ until p g ((maxX, maxY), "\n")
    p (pos, _) = pos == (maxX, minY-1)
    g (pos@(posX, posY), acc)
        | posX == minX = ((maxX, posY-1), '\n':acc2)
        | otherwise    = ((posX-1, posY), acc2) where
        acc2 = h mCell : acc
        mCell = mylookup w pos
    h (Just (cand, cell))
        | not cell  = '.'
        | cand      = 'x'
        | otherwise = 'X'
    h Nothing = ' '
    header = show frame ++ "\n"
    frame@(minX, minY, maxX, maxY) = getFrame $ mykeys w

data Statistic = Statistic {
    statistic :: CGL.Statistic,
    minXCoord :: Int,
    minYCoord :: Int,
    maxXCoord :: Int,
    maxYCoord :: Int }

instance Show Statistic where
    show s =
        show (statistic s) ++
        "min x = " ++ show (minXCoord s) ++ "\n" ++
        "min y = " ++ show (minYCoord s) ++ "\n" ++
        "max x = " ++ show (maxXCoord s) ++ "\n" ++
        "max y = " ++ show (maxYCoord s) ++ "\n"

myFoldr :: World -> ((Bool, Cell) -> CGL.Statistic -> CGL.Statistic) -> CGL.Statistic -> CGL.Statistic
myFoldr (Strict cells) = \f a1 -> SMap.foldr f a1 cells
myFoldr (Lazy   cells) = \f a1 -> LMap.foldr f a1 cells

getStatistic :: World -> Statistic
getStatistic w = Statistic stat minX minY maxX maxY where
    stat                     = CGL.getStatistic False $ myFoldr w
    (minX, minY, maxX, maxY) = getFrame $ mykeys w

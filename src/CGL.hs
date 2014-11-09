module CGL (
    nextTick,
    initialize,
    Statistic(..),
    getStatistic
    ) where

-- evolve cells and candidates one tick
-- input: map of coordinate to pair of candidate flag and cell 
-- the candidates flag indicates if a cell is checked for evolution in next tick
-- non candidates are not re-evaluated
-- a candidate is a cell for which at least one neighbour has changed
-- output: next generation map
-- promise: empty cells in map are candidates
nextTick :: Eq a =>
    a -> -- defaultCell
    c -> -- emptyWorld
    (k -> [k]) -> -- get neighbourhood of cells
    (k -> a -> [(k,a)] -> a) -> -- evolve
    ((Maybe (Bool, a) -> Maybe (Bool, a)) -> k -> c -> c) -> -- alter
    ((Bool, a) -> k -> c -> (Bool, a)) -> -- find with default
    ((k -> (Bool, a) -> c -> c) -> c -> c -> c) -> -- foldr with key
    c -> -- current world
    c    -- new world
nextTick 
    defaultCell
    emptyWorld
    getNbh 
    evolve
    alter
    findWithDefault
    foldrWithKey
    cells 
    = foldrWithKey f emptyWorld cells where
    f coord (cand, cell) acc
        -- unchanged and non-empty? see def
        | not changed && cell /= defaultCell = acc2
        -- changed? same as above plus all neighbours
        | changed  = foldr f2 acc2 nbs
        -- otherwise skip this cell
        | otherwise = acc where
            -- update accumulated cells
            acc2 = alter f3 coord acc where
                --f3 :: Maybe (Bool, a) -> Maybe (Bool, a)
                -- insert as non-candidate when not already inserted
                f3 Nothing = Just (False, newCell)
                -- replace cell when found
                f3 (Just (x, _)) = Just (x, newCell)
            -- insert as candidate cell if cell changed
            f2 (cand2, _) = alter f3 cand2 where
                f3 Nothing       = Just (True, defaultCell)
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
                f3 coord2  = (coord2, snd (findWithDefault (False, defaultCell) coord2 cells))

-- initialize cell map from cell list
initialize :: 
    a -> -- defaultCell
    (k -> [k]) -> -- get neighbourhood of cells
    ((Maybe (Bool, a) -> Maybe (Bool, a)) -> k -> c -> c) -> -- alter
    ([(k, (Bool, a))] -> c) -> -- from list
    [(k, a)] -> 
    c
-- accumulate map from all cells and their neighbours starting with empty map
initialize 
    defaultCell
    getNbh
    alter
    fromList
    coordCells = foldr f cells nbs where
    -- insert candidate cell when not in already
    f = alter g where
        g Nothing = Just (True, defaultCell)
        g x       = x
    -- add neighbours to cell list
    nbs = concatMap getNbh coords
    cells = fromList $ zip coords (zip (repeat True) cs)
    (coords, cs) = unzip coordCells

data Statistic = Statistic {
    cellCount :: Int,
    nonDefaultCount :: Int,
    candidateCount :: Int } 

instance Show Statistic where
    show s = 
        let cc = cellCount s 
            ndc = nonDefaultCount s
            cac = candidateCount s
        in "cells             = " ++ show cc ++ "\n" ++
           "non default cells = " ++ show ndc ++ " (" ++ show (div (100*ndc) cc) ++ "%)\n" ++
           "candidate cells   = " ++ show cac ++ " (" ++ show (div (100*cac) cc) ++ "%)\n"

initStatistic :: Statistic
initStatistic = Statistic 0 0 0

getStatistic :: Eq a =>
    a -> -- defaultCell
    (((Bool, a) -> Statistic -> Statistic) -> Statistic -> Statistic) -> -- foldr
    Statistic
getStatistic
    defaultCell
    myfoldr = myfoldr f initStatistic where
        f (cand, cell) stat = Statistic clc ndc cdc where
            clc = 1 + cellCount stat
            ndc
                | cell /= defaultCell = 1 + nonDefaultCount stat
                | otherwise           = nonDefaultCount stat
            cdc
                | cand      = 1 + candidateCount stat
                | otherwise = candidateCount stat

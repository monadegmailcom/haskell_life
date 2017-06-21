-- | Main entry point to the application.
module Main where

import qualified Conway

import Control.Monad
import Data.List

run :: Conway.World -> Int -> Conway.World
run = genericIndex . iterate Conway.nextTick

interactive :: Conway.World -> Int -> IO ()
interactive w n = do
    print n
    putStr $ Conway.draw w
    input <- getChar
    unless (input == 'q') $ interactive (Conway.nextTick w) (n+1)

-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Game of life"
    -- floater = [(0,2),(1,2),(2,2),(2,1),(1,0)]
    -- block = [(0,0),(0,1),(1,0),(1,1)]
    -- blinker = [(0,0), (0,1),(0,2)]

    let rpentomino = Conway.initialize Conway.LazyMap [(1,0),(2,0),(0,1),(1,1),(1,2)]
    let n = 1000
    let w = run rpentomino n
    putStr $ show (Conway.getStatistic w)
    --putStr $ Conway.draw w

--    interactive rpentomino 0

import Graphics.UI.GLUT 
import Data.IORef
import qualified Data.Map.Lazy as LMap

-- coordinate >= 0 to position in [-1,1]
c2p :: Fractional a => a -> a -> a
c2p c n = -1+2*c/n

-- position in [0,1] to coordinate >= 0
p2c :: RealFrac a => a -> a -> Int 
p2c p n = floor $ n*(p+1)/2

-- fill grid n >= 0 and cell (x,y) with spacing r >= 0
drawRect :: GLfloat -> Int -> Int -> Int -> IO ()
drawRect r n x y = rect v1 v2 where
   v1  = Vertex2 ( r + f rx    ) ( r + f ry    )
   v2  = Vertex2 (-r + f (rx+1)) (-r + f (ry+1))
   f t = c2p t rn
   rx  = fromIntegral x
   ry  = fromIntegral y
   rn  = fromIntegral n

-- draw grid n > 0
drawGrid :: Int -> IO ()
drawGrid n = renderPrimitive Lines $
   mapM_ (\(x, y) -> vertex $ Vertex2 x y) (hl ++ vl) where
      hl = [ (x  , f y) | y <- [0..rn], x <- [-1,1]]
      vl = [ (f x, y  ) | x <- [0..rn], y <- [-1,1]]
      f r = c2p r rn
      rn  = fromIntegral n :: GLfloat

type MapRef = IORef (LMap.Map (Int,Int) Bool)

main :: IO ()
main = do
  -- reference var for set grid cells
  cs <- newIORef LMap.empty :: IO MapRef 
  -- grid size
  let n = 10 :: Int
  -- grid spacing for cell fill
  let r = 0.01 :: GLfloat
  -- window caption
  let c = "2 Dim Grid"
  -- initialize app and window
  (_progName, _args) <- getArgsAndInitialize
  _window            <- createWindow c
  -- set callbacks
  displayCallback $= display r n cs 
  reshapeCallback $= Just reshape 
  mouseCallback   $= Just (mouse n cs)
  -- enter mainloop
  mainLoop

-- reshape with constraints on minimum size and quadratic aspect ratio
reshape :: ReshapeCallback
reshape s@(Size x y) = do 
  Size sx sy <- get screenSize
  windowSize $= f sx sy 
  viewport   $= (Position 0 0, s) where
    f sx sy = Size m m where
      m = max (min x y) (min (div sx 10) (div sy 10))

-- mouse event callback, fill grid cell on left click
-- r >= 0: cell fill spacing
-- n > 0:  grid size
mouse :: Int -> MapRef -> MouseCallback
mouse n cs LeftButton Down (Position px py) = do
  Size sx sy <- get windowSize
  -- calc coord from pos
  modifyIORef cs $ LMap.alter h (g px py sx sy)
  postRedisplay Nothing where
    h Nothing = Just True
    h _       = Nothing
    g x y sx sy = (f u, f v) where 
      -- calc pos in [-1,1] from mouse click pos
      u = -1+2*x'/sx'
      v =  1-2*y'/sy'
      -- convert integrals to floats
      [x',y',sx',sy'] = map fromIntegral [x,y,sx,sy] :: [GLfloat]
      f               = flip p2c $ fromIntegral n
mouse _ _ _ _ _ = return ()

-- display grid and set cells
display :: GLfloat -> Int -> MapRef -> DisplayCallback
display r n cs = do 
  clear [ColorBuffer]
  drawGrid n  
  m <- get cs 
  mapM_ (uncurry $ drawRect r n) (LMap.keys m) 
  flush

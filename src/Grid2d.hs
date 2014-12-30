import Graphics.UI.GLUT 

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
   mapM_ (\(x, y) -> vertex $ Vertex2 x y) (hLines ++ vLines) where
      hLines = [ (x  , f y) | y <- [0..rn], x <- [-1,1]]
      vLines = [ (f x, y  ) | x <- [0..rn], y <- [-1,1]]
      f r    = c2p r rn
      rn     = fromIntegral n :: GLfloat

main :: IO ()
main = do
  -- grid size
  let n  = 10   :: Int
  -- grid spacing for cell fill
  let r  = 0.01 :: GLfloat
  -- window caption
  let c  = "2 Dim Grid"
  (_progName, _args) <- getArgsAndInitialize
  _window            <- createWindow c
  displayCallback $= display r n [] 
  reshapeCallback $= Just reshape 
  mouseCallback   $= Just (mouse r n)
  mainLoop

-- reshape with constraints on minimum size and quadratic aspect ratio
reshape :: ReshapeCallback
reshape s@(Size x y) = do 
  Size sx sy <- get screenSize
  let m = max (min x y) (min (div sx 10) (div sy 10))
  let n = Size m m 
  windowSize $= n
  viewport   $= (Position 0 0, s)

-- mouse event callback, fill grid cell on left click
-- r >= 0: cell fill spacing
-- n > 0:  grid size
mouse :: GLfloat -> Int -> MouseCallback
mouse r n LeftButton Down (Position px py) = do
  -- calc pos in [-1,1] from mouse click pos
  Size sx sy <- get windowSize
  let x  = -1 + 2 * (fromIntegral px / fromIntegral sx)
  let y  =  1 - 2 * (fromIntegral py / fromIntegral sy)
  -- calc coord from pos
  let rn = fromIntegral n :: GLfloat
  let cx = p2c x rn
  let cy = p2c y rn
  -- fill cell
  drawRect r n cx cy 
  flush 
mouse _ _ _ _ _ = return ()

display :: GLfloat -> Int -> [(Int,Int)] -> DisplayCallback
display r n cs = do 
  clear [ColorBuffer]
  drawGrid n  
  mapM_ (uncurry $ drawRect r n) cs 
  flush

import Graphics.UI.GLUT 
import Data.IORef
import qualified Data.Set as Set 
import Control.Monad (liftM)

-- coordinate to viewport position 
-- (0,0) -> (-1,1), (n,n) -> (1,-1)
c2p :: (Int,Int) -> (Int,Int) -> (GLfloat,GLfloat)
c2p (cx,cy) (n,k) = (f cx n,-f cy k) where
  f c m = -1+2*c'/m' where
    c' = fromIntegral c
    m' = fromIntegral m

-- viewport position to coordinate 
p2c :: (GLfloat,GLfloat) -> (Int,Int) -> (Int,Int)
p2c (px,py) (n,k) = (f px n,k-1-f py k) where
  f p m = floor $ m'*(p+1)/2 where
    m' = fromIntegral m

-- fill cell with spacing 
drawRect :: (Int,Int) -> (Int,Int) -> IO () 
drawRect d c@(cx,cy) = rect v1 v2 where
   v1 = uncurry Vertex2 $ c2p c d
   v2 = uncurry Vertex2 $ c2p (cx+1,cy+1) d
   
-- draw grid 
drawGrid :: (Int,Int) -> IO ()
drawGrid d@(n,k) = renderPrimitive Lines $
   mapM_ (\(x, y) -> vertex $ Vertex2 x y) (hl ++ vl) where
      hl = [ c2p (x,y) d | y <- [0..k], x <- [0,n]]
      vl = [ c2p (x,y) d | x <- [0..n], y <- [0,k]]

-- cell coordinate map
type Coords = Set.Set (Int,Int) 

toSize :: Int -> Int -> Size
toSize x y = Size (fromIntegral x) (fromIntegral y)
fromSize :: Size -> (Int,Int)
fromSize (Size x y) = (fromIntegral x,fromIntegral y)
fromPosition :: Position -> (Int,Int)
fromPosition (Position x y) = (fromIntegral x,fromIntegral y)

main :: IO ()
main = do
  -- initialize app 
  (_progName, _args) <- getArgsAndInitialize

  -- create window caption
  let cCaption = "Conways's game of life"
  _window <- createWindow cCaption

  -- get current window size
  Size wx wy <- get windowSize

  -- default number of cells in x dim
  let cdefX = 10 

  -- get number of cells in x and y dim and width in pixels
  let (n,k,w) = getDim cdefX wx wy :: (Int,Int,Int)
      
  -- resize current window
  windowSize $= toSize (n*w) (k*w) 

  -- reference var for grid dimension 
  dim <- newIORef (n,k,w) :: IO (IORef (Int,Int,Int))

  -- reference var for grid cell set
  cs <- newIORef Set.empty :: IO (IORef Coords)

  -- set callbacks
  displayCallback $= display dim cs 
  reshapeCallback $= Just (reshape dim)
  mouseCallback   $= Just (mouse dim cs)

  -- enter mainloop
  mainLoop 

  where
    getDim n wx wy = (n',k,w) where
      k = max 1 $ div (n'*wy') wx' 
      w = max 1 $ div wx' n' 
      wx' = fromIntegral $ max 1 wx 
      wy' = fromIntegral wy
      n'  = max 1 n

-- resize dim and window 
reshape :: IORef (Int,Int,Int) -> ReshapeCallback
reshape dim s = do 
  -- resize dim according to resized window
  modifyIORef dim f

  -- resize window accoring to dim
  (n,k,w)    <- get dim
  windowSize $= toSize (n*w) (k*w) 

  -- set new viewport
  viewport   $= (Position 0 0, s) 

  where
    f (_,_,w) = (g sx,g sy,w) where
      g = max 1 . flip div w
      (sx,sy) = fromSize s

-- mouse event callback, toggle grid cell on left click
mouse :: IORef (Int,Int,Int) -> IORef Coords -> MouseCallback
mouse dim cs LeftButton Down pos = do
  -- calc coord from pos
  s <- liftM fromSize $ get windowSize
  let w = fromPosition pos
  (n,k,_) <- get dim
  modifyIORef cs $ f (n,k) s w 
  
  postRedisplay Nothing 
  
  where
    -- toggle cell membership in set
    f d s w set   
      | Set.member c set = Set.delete c set  
      | otherwise        = Set.insert c set where
        c = p2c p d
        -- calc viewport pos from mouse click pos
        p = c2p w s 
mouse _ _ _ _ _ = return ()

-- display grid and set cells
display :: IORef (Int,Int,Int) -> IORef Coords -> DisplayCallback
display dim cs = do 
  clear [ColorBuffer]
  d <- liftM (\(n,k,_) -> (n,k)) $ get dim
  drawGrid d 
  s <- get cs 
  mapM_ (drawRect d) (Set.elems s) 
  flush

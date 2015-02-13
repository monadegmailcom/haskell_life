import Graphics.UI.GLUT as Gl
import Data.IORef
import qualified Data.Set as Set 
import Control.Monad (when) 
import Data.Maybe (fromJust,isJust)

-- Display 2-dim cell grid as orthogonal projection without perspective
-- distortion. The grid is an unbounded equally spaced grid of width 1. The
-- viewing rectangle has exactly the size of the window with an optional 
-- offset in x and y direction. A variable scaling factor determines the
-- viewing volume. The user can interactively resize the window or zoom in
-- and out to change the viewing volume size and move the underlying grid
-- by dragging. Cells can be set and removed from the grid.

-- fill cell 
drawRect :: (Int,Int) -> IO () 
drawRect (cx,cy) = rect v1 v2 where
   v1 = Vertex2 x y
   v2 = Vertex2 (x+1) (y+1)
   x = fromIntegral cx :: GLfloat
   y = fromIntegral cy :: GLfloat
    
-- left right bottom top
drawGrid :: Int -> Int -> Int -> Int -> IO ()
drawGrid l r b t = renderPrimitive Lines $ mapM_ f (hl ++ vl) where
  f  = vertex . uncurry Vertex2
  -- horizontal lines
  hl = merge p1 p2
  p1 = zip (repeat $ fromIntegral l) r1 
  p2 = zip (repeat $ fromIntegral r) r1 
  r1 = map fromIntegral [b..t] :: [GLfloat]
  -- vertical lines
  vl = merge q1 q2
  q1 = zip r2 (repeat $ fromIntegral b)
  q2 = zip r2 (repeat $ fromIntegral t)
  r2 = map fromIntegral [l..r] :: [GLfloat] 

  merge [] ys = ys
  merge (x:xs) ys = x:merge ys xs
  
-- cell coordinate set 
type Coords = Set.Set (Int,Int) 

-- left, top, scale
data Grid = Grid GLdouble GLdouble GLdouble
-- save mouse position and grid state at the moment of a mouse click.
-- the boolean parameter is False on a click-and-motion event, True
-- otherwise.
data MotionMode = Motion | Zoom deriving Eq
data MotionState = MotionState Position Grid MotionMode Bool 

main :: IO ()
main = do
  -- initialize app 
  (_progName, _args) <- getArgsAndInitialize

  -- create window caption
  let cCaption = "Conways's game of life"
  _window <- createWindow cCaption

  -- get current window size
  Size wx _ <- get windowSize

  -- default number of cells in x dim
  let cdefX = 10 :: Int
 
  let sc = fromIntegral wx / fromIntegral cdefX :: GLdouble

  -- reference var for grid
  gridRef <- newIORef (Grid 0 0 sc) :: IO (IORef Grid)

  -- reference var for grid cell set
  csRef <- newIORef Set.empty :: IO (IORef Coords)

  -- reference for initial mouse move position
  msRef <- newIORef Nothing :: IO (IORef (Maybe MotionState))

  -- set callbacks
  displayCallback       $= display gridRef csRef
  motionCallback        $= Just (motion msRef gridRef)
  keyboardMouseCallback $= Just (keyboardMouse msRef gridRef csRef)

  -- enter mainloop
  mainLoop 

toggle :: IORef Grid -> IORef Coords -> Position -> IO ()
toggle gridRef csRef (Position px py) = do
  -- calc coord from pos
  Grid l t sc <- get gridRef
  let c = (floor $ (l+fromIntegral px)/sc,
           floor $ (t-fromIntegral py)/sc)
  modifyIORef csRef $ f c 
  postRedisplay Nothing 
  where
    -- toggle cell membership in set
    f c set   
      | Set.member c set = Set.delete c set  
      | otherwise        = Set.insert c set 

keyboardMouse :: IORef (Maybe MotionState) -> IORef Grid -> IORef Coords
  -> KeyboardMouseCallback
keyboardMouse msRef gridRef _ (MouseButton LeftButton) Down 
              (Modifiers Up Up Up) pos = do
  grid <- get gridRef
  modifyIORef msRef $ const (Just (MotionState pos grid Motion True))
keyboardMouse msRef gridRef _ (MouseButton LeftButton) Down 
              _ pos = do
  grid <- get gridRef
  modifyIORef msRef $ const (Just (MotionState pos grid Zoom True)) 
keyboardMouse msRef g cs (MouseButton LeftButton) Up _ pos = do
  ms <- get msRef
  modifyIORef msRef $ const Nothing 
  when (isJust ms) $ do
    let MotionState _ _ _ clicked = fromJust ms
    when clicked  $ toggle g cs pos 
keyboardMouse _ _ _ _ _ _ _ = return ()

motion :: IORef (Maybe MotionState) -> IORef Grid -> MotionCallback
motion msRef gridRef (Position px py) = do 
  ms <- get msRef
  when (isJust ms) $ do
    let MotionState p@(Position mx my) g@(Grid l t sc) mode _ = fromJust ms 
    let (dx,dy) = (fromIntegral (mx-px),fromIntegral (py-my)) 
    if mode == Motion
      then gridRef $~ const (Grid (l+dx) (t+dy) sc) 
      else do
        let f = exp (0.01*dy)
        Size wx wy <- get windowSize 
        let x = l*f-(1-f)*fromIntegral wx/2
        let y = t*f+(1-f)*fromIntegral wy/2
        gridRef $~ const (Grid x y (f*sc))
    modifyIORef msRef $ const (Just (MotionState p g mode False))
    postRedisplay Nothing 

prepareProjection :: Grid -> IO ()
prepareProjection (Grid left top sc) = do
  Size sx sy <- get windowSize
  let right  = left + fromIntegral sx
  let bottom = top - fromIntegral sy

  -- prepare projection
  matrixMode $= Projection
  loadIdentity
  ortho2D left right bottom top
  scale sc sc 1.0

prepareModelview :: Grid -> Coords -> IO ()
prepareModelview (Grid left top sc) cs = do
  clear [ColorBuffer]

  -- prepare model view
  matrixMode $= Modelview 1 
  loadIdentity
  lookAt (Vertex3 left top 1) (Vertex3 left top 0) (Vector3 0 1 0) 
  Size wx wy <- get windowSize
  let l = floor $ left/sc
  let r = ceiling $ (left+fromIntegral wx)/sc
  let b = floor $ (top-fromIntegral wy)/sc
  let t = ceiling $ top/sc
  drawGrid l r b t 

  -- filter cells that fall in viewing volume
  let cells = filter (\(x,y) -> l <= x && x <= r && b <= y && y <= t) 
              $ Set.elems cs
  mapM_ drawRect cells 

display :: IORef Grid -> IORef Coords -> DisplayCallback
display gridRef csRef = do
  -- update grid
  g <- get gridRef

  prepareProjection g

  cs <- get csRef
  prepareModelview g cs

  flush 


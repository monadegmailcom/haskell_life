import Graphics.UI.GLUT 

drawRect :: Int -> Int -> Int -> IO ()
drawRect n x y = rect v1 v2 where
   v1 = Vertex2 (-1+r+2*rx/rn) (-1+r+2*ry/rn)
   v2 = Vertex2 (-1-r+2*(rx+1)/rn) (-1-r+2*(ry+1)/rn)
   rx = fromIntegral x 
   ry = fromIntegral y
   rn = fromIntegral n
   r  = 0.01 :: GLfloat

drawGrid :: Int -> IO ()
drawGrid n = renderPrimitive Lines $
   mapM_ (\(x, y) -> vertex $ Vertex2 x y) (hLines ++ vLines) where
      hLines = [ (x, -1+2*y/rn) | y <- [0..rn], x <- [-1,1]] 
      vLines = [ (-1+2*x/rn, y) | x <- [0..rn], y <- [-1,1]] 
      rn = fromIntegral n :: GLfloat

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  mainLoop
 
display :: DisplayCallback
display = do 
  clear [ColorBuffer]
  let n = 10
  drawGrid n  
  drawRect n 2 1
  flush

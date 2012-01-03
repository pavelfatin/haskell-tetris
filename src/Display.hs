module Display (drawScene) where

import Control.Monad
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Data
import Scene
import Stats

drawScene :: Scene -> IO ()
drawScene scene = do
    loadIdentity
    clear [ColorBuffer]
    preservingMatrix $ drawBlocks (areaOfScene scene) (blocksInScene scene)
    drawStats $ statsInScene scene
    when (not $ isValidScene scene) drawGameOver
    flush
    swapBuffers

drawBlocks :: Area -> [Block] -> IO ()
drawBlocks (Area width height) blocks = do
    translate (Vector3 (-1.0) (-1.0) (0.0 :: GLfloat))
    scale (2.0 / fromIntegral width) (2.0 / fromIntegral height) (1.0 :: GLfloat)
    mapM_ (drawBlock height) blocks

drawBlock :: Int -> Block -> IO ()
drawBlock height (Block (Location x y) (Coloring r g b)) = do
    let i = 0.03
        xx = fromIntegral x
        yy = fromIntegral (height - y)
    color $ (Color3 (r :: GLfloat) g b)
    rect (Vertex2 ((xx + i) :: GLfloat) (yy - 1.0 + i)) (Vertex2 (xx + 1.0 - i) (yy - i))

drawStats :: Stats -> IO ()
drawStats stats = do
    color $ (Color3 (0.7 :: GLfloat) 0.7 0.7)
    drawText (-0.95) 0.9 $ "Level: " ++ (show $ levelInStats stats)
    drawText (-0.95) 0.8 $ "Lines: " ++ (show $ linesInStats stats)
    drawText (-0.95) 0.7 $ "Score: " ++ (show $ scoreInStats stats)

drawText :: Float -> Float -> String -> IO ()
drawText x y s =
    preservingMatrix $ do
        translate (Vector3 x y (0.0 :: GLfloat))
        scale 0.0008 0.0005 (1.0 :: GLfloat)
        renderString MonoRoman s

drawGameOver :: IO ()
drawGameOver =
    preservingMatrix $ do
        color $ (Color3 (0.8 :: GLfloat) 0.8 0.8)
        rect (Vertex2 ((-0.9) :: GLfloat) 0.35) (Vertex2 0.9 0.15)
        color $ (Color3 (0.7 :: GLfloat) 0.2 0.2)
        translate (Vector3 (-0.83) 0.2 (0.0 :: GLfloat))
        scale 0.0018 0.001 (1.0 :: GLfloat)
        renderString MonoRoman "Game Over"
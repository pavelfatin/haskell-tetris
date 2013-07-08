module Main (main) where

import Control.Monad
import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Random

import Data
import Display
import Piece
import Scene
import Stats

gameArea :: Area
gameArea = Area 10 20

main = do
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    createWindow "Haskell-blocks"
    windowSize $= Size (fromIntegral (width gameArea) * 25) (fromIntegral (height gameArea) * 25)
    startNewGame
    mainLoop

startNewGame :: IO ()
startNewGame = do
    sceneRef <- newIORef $ createEmptyScene gameArea
    displayCallback $= drawingCallback sceneRef
    keyboardMouseCallback $= Just (inputCallback sceneRef)
    timerCallback sceneRef

drawingCallback :: IORef Scene -> IO ()
drawingCallback sceneRef = get sceneRef >>= drawScene

timerCallback :: IORef Scene -> TimerCallback
timerCallback sceneRef = do
    scene <- get sceneRef
    when (isValidScene scene) $ do
        nextScene <- nextScene scene
        sceneRef $= nextScene
        postRedisplay Nothing
        addTimerCallback (delayForScene nextScene) (timerCallback sceneRef)

delayForScene :: Scene -> Int
delayForScene scene =
    if isPiecePresentInScene scene then
        500 - 20 * (levelInStats $ statsInScene scene) else 0

nextScene :: Scene -> IO Scene
nextScene scene =
    if isPiecePresentInScene scene then
        return $ lowerPieceInScene scene
    else do
        piece <- nextPiece
        return $ addPieceToScene piece scene

nextPiece :: IO Piece
nextPiece = choiceOneOf pieces
    where choiceOneOf :: [a] -> IO a
          choiceOneOf xs = randomRIO (0, (length xs - 1)) >>= return . (xs !!)

inputCallback :: IORef Scene -> KeyboardMouseCallback
inputCallback sceneRef (SpecialKey key) Down _ _ = do
    scene <- get sceneRef
    if isValidScene scene then do
        sceneRef $= reactionOn key scene
        postRedisplay Nothing
        else when (key == KeyDown) startNewGame 

inputCallback _ _ _ _ _ = return ()

reactionOn :: SpecialKey -> Scene -> Scene
reactionOn KeyLeft = movePieceLeftInScene
reactionOn KeyRight = movePieceRightInScene
reactionOn KeyUp = rotatePieceInScene
reactionOn KeyDown = dropPieceInScene
reactionOn _ = id
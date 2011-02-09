module Stuff (display, reshape, keyboardMouse, motion, makeUhh) where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT 
import Data.IORef

import Display
import Uhh

kmMoveAction f uhh = 
    modifyIORef uhh (\u -> Uhh (f $ uhhCamPos u) (uhhLookAt u) )
    
keyboardMouse u (Char ' ') Down _ _ = do
    uhh <- readIORef u
    putStrLn $ show uhh
keyboardMouse uhh (Char 'a') Down _ _ = kmMoveAction vecLeft uhh
keyboardMouse uhh (Char 'd') Down _ _ = kmMoveAction vecRight uhh
keyboardMouse uhh (Char 'q') Down _ _ = kmMoveAction vecUp uhh
keyboardMouse uhh (Char 'e') Down _ _ = kmMoveAction vecDown uhh
keyboardMouse uhh key state modifiers position = do
    putStrLn $ show key

reshape s@(Size w h) = do
	matrixMode $= Projection
	loadIdentity
	viewport $= (Position 0 0, s)
	perspective 45 ratio 0.001 1000
	where
	    ratio = (fromIntegral w) / (fromIntegral h)
	    
motion p@(Position x y) = return ()

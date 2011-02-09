module Stuff (display, reshape, keyboardMouse, motion) where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT 
import Data.IORef

import Display
import Uhh

kmMoveAction f uhh = 
    modifyIORef uhh (\u -> u { uhhCamPos = f $ uhhCamPos u } )
    
-- keyboardMouse
keyboardMouse u (Char ' ') Down _ _ = do
    uhh <- readIORef u
    putStrLn $ show uhh
keyboardMouse uhh (Char 'a') Down _ _ = kmMoveAction vecLeft uhh
keyboardMouse uhh (Char 'd') Down _ _ = kmMoveAction vecRight uhh
keyboardMouse uhh (Char 'q') Down _ _ = kmMoveAction vecUp uhh
keyboardMouse uhh (Char 'e') Down _ _ = kmMoveAction vecDown uhh
keyboardMouse uhh (Char 'w') Down _ _ = kmMoveAction vecForward uhh
keyboardMouse uhh (Char 's') Down _ _ = kmMoveAction vecBackward uhh

keyboardMouse uhh (Char '<') Down _ _ = do
    modifyIORef uhh (\u -> u { uhhSteps = (max 2 $ (uhhSteps u)-1) } )
    updateUhh uhh
keyboardMouse uhh (Char '>') Down _ _ = do
    modifyIORef uhh (\u -> u { uhhSteps = (uhhSteps u) + 1 } )
    updateUhh uhh

keyboardMouse uhh (Char '2') Down _ _ = do
    modifyIORef uhh (\u -> u { uhhWireframe = not $ uhhWireframe u } )
    updateUhh uhh

keyboardMouse uhh key Down modifiers position = do
    putStrLn $ show key
    
keyboardMouse uhh key state modifiers position = return ()
-- keyboardMouse

reshape s@(Size w h) = do
	matrixMode $= Projection
	loadIdentity
	viewport $= (Position 0 0, s)
	perspective 45 ratio 0.001 1000
	where
	    ratio = (fromIntegral w) / (fromIntegral h)
	    
motion p@(Position x y) = return ()

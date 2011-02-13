module Stuff (display, reshape, keyboardMouse, motion) where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT 
import Data.IORef

import Display
import Uhh
import Vec

kmMoveAction f uhhref = do
    modifyIORef uhhref (\uhh -> uhh { uhhCamPos = f $ uhhCamPos uhh } )
    
kmFunnies i uhhref = do
    modifyUhh uhhref (\uhh -> uhh {
        uhhFunnyIndex = (uhhFunnyIndex uhh + i) `mod` length uhhFunnies })
    
keyboardMouse uhhref (Char 'r') Down _ _ = kmFunnies 1 uhhref
keyboardMouse uhhref (Char 'f') Down _ _ = kmFunnies (-1) uhhref

-- keyboardMouse
keyboardMouse uhhref (Char 'a') Down _ _ = kmMoveAction (flip(-) vecX) uhhref
keyboardMouse uhhref (Char 'd') Down _ _ = kmMoveAction (+ vecX) uhhref
keyboardMouse uhhref (Char 'q') Down _ _ = kmMoveAction (+ vecY) uhhref
keyboardMouse uhhref (Char 'e') Down _ _ = kmMoveAction (flip (-) vecY) uhhref
keyboardMouse uhhref (Char 'w') Down _ _ = kmMoveAction (+ vecZ) uhhref
keyboardMouse uhhref (Char 's') Down _ _ = kmMoveAction (flip (-) vecZ) uhhref

keyboardMouse uhhref (Char '<') Down _ _ =
    modifyUhh uhhref
    (\uhh -> uhh { uhhSteps = (max 2 $ (uhhSteps uhh)-1) })
keyboardMouse uhhref (Char '>') Down _ _ =
    modifyUhh uhhref
    (\uhh -> uhh { uhhSteps = (uhhSteps uhh) + 1 })

keyboardMouse uhhref (Char '2') Down _ _ = do
    modifyUhh uhhref (\uhh -> uhh { uhhWireframe = not $ uhhWireframe uhh } )

keyboardMouse uhhref key Down modifiers position = do
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

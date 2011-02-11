import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

import Uhh
import Stuff
import Vec

main = do
    (progname, _) <- getArgsAndInitialize
    initialDisplayMode $= [ RGBAMode, WithDepthBuffer ]
    createWindow progname
    depthFunc $= Just Less
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    blend $= Enabled
    
    uhh <- newUhh 
        (vec (-10) 10 (-10))
        (vec 0 0 0)
        (9)
    uhhref <- newIORef uhh
    
    displayCallback $= display uhhref
    idleCallback $= Just (display uhhref)
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboardMouse uhhref)
    motionCallback $= Just motion
    mainLoop


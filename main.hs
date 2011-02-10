import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

import Uhh
import Stuff

main = do
    (progname, _) <- getArgsAndInitialize
    initialDisplayMode $= [ RGBAMode, WithDepthBuffer ]
    createWindow progname
    depthFunc $= Just Less
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    blend $= Enabled
    
    uhh <- newUhh 
        (makeVec (-10) 10 (-10))
        (makeVec 0 0 0)
        (9)
    uhhref <- newIORef uhh
    
    displayCallback $= display uhhref
    idleCallback $= Just (display uhhref)
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboardMouse uhhref)
    motionCallback $= Just motion
    mainLoop


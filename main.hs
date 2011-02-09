import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

import Uhh
import Stuff

main = do
    uhh <- newIORef (makeUhh 
        (makeVec (-10) 10 (-10))
        (makeVec 0 0 0)
        (4))
    (progname, _) <- getArgsAndInitialize
    initialDisplayMode $= [ RGBAMode, WithDepthBuffer ]
    createWindow progname
    depthFunc $= Just Less
    displayCallback $= display uhh
    idleCallback $= Just (display uhh)
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboardMouse uhh)
    motionCallback $= Just motion
    mainLoop


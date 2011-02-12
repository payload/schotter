import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

import Data.Vec.Packed
import qualified Data.Vec as Vec
import Vec

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
        (Vec3D (-10) 10 (-10))
        (Vec3D 0 0 0)
        (40)
    uhhref <- newIORef uhh
    
    displayCallback $= display uhhref
    --idleCallback $= Just (display uhhref)
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboardMouse uhhref)
    motionCallback $= Just motion
    --mainLoop
    putStrLn $ show uhh


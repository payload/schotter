import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Data.Vec.Packed

import Uhh
import Stuff

lightAmbient = Color4 a a a 1
    where a = 0.2
lightDiffuse = Color4 x x x 1
    where x = 1
lightSpecular = Color4 0 0 1 1

main = 
    let
        light0 = Light 0
    in do
    (progname, _) <- getArgsAndInitialize
    initialDisplayMode $= [ RGBAMode, WithDepthBuffer ]
    initialWindowSize $= Size 800 500
    createWindow progname
    depthFunc $= Just Less
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    blend $= Enabled
    
    ambient light0 $= lightAmbient
    diffuse light0 $= lightDiffuse
    --light light0 $= Enabled
    --lighting $= Enabled
    --shadeModel $= Flat
    
    uhh <- newUhh 
        (Vec3F (-15) 15 (-15))
        (Vec3F 0 0 0)
    uhhref <- newIORef uhh
    updateUhh uhhref
    
    displayCallback $= display uhhref
    addTimerCallback 50 $ timestep uhhref
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboardMouse uhhref)
    motionCallback $= Just motion
    mainLoop

timestep uhhref = do
    modifyUhh uhhref (uhhSimulate (50/1000))
    display uhhref
    addTimerCallback 50 $ timestep uhhref

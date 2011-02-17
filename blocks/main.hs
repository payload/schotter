import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

import Data.Vec.Packed
import qualified Data.Vec as Vec
import Vec

import Uhh
import Simulator
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
        (Vec3D (-10) 10 (-10))
        (Vec3D 0 0 0)
    uhhref <- newIORef uhh
    updateUhh uhhref
    
    displayCallback $= display uhhref
    idleCallback $= Just (idle uhhref)
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboardMouse uhhref)
    motionCallback $= Just motion
    mainLoop

idle uhhref = do
    modifyUhh uhhref (\uhh -> uhh { uhhSimStuff = simulate (uhhSimStuff uhh) } )
    display uhhref

module Display (display) where
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GLU
import Graphics.UI.GLUT
import GHC.Float
import Data.IORef

import Cube
import Utils
import Uhh

display u = do
    uhh <- readIORef u
    clear [ ColorBuffer, DepthBuffer ]
    matrixMode $= (Modelview 0)
    loadIdentity
    lookAt 
        (vec2Vertex3 $ uhhCamPos uhh)
        (Vertex3 (0::GLdouble) 0 0)
        (Vector3 (0::GLdouble) 1 0)
    render
    flush
        
render = do
    renderObject Solid (Teapot 2)


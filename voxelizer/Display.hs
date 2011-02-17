module Display (display) where
import Graphics.Rendering.OpenGL
import Data.IORef

import Uhh
import Vec

display uhhref = do
    uhh <- readIORef uhhref
    clear [ ColorBuffer, DepthBuffer ]
    matrixMode $= (Modelview 0)
    loadIdentity
    lookAt 
        (vec2Vertex3 $ uhhCamPos uhh)
        (Vertex3 (0::GLdouble) 0 0)
        (Vector3 (0::GLdouble) 1 0)
    render uhh
    flush
        
render uhh = do
    callList $ uhhDisplayList uhh


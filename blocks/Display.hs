module Display (display) where
import Graphics.Rendering.OpenGL
import Data.IORef

import Uhh

import Data.Vec.Packed
import Vec

display uhhref = do
    uhh <- readIORef uhhref
    clear [ ColorBuffer, DepthBuffer ]
    matrixMode $= (Modelview 0)
    loadIdentity
    lookAt 
        (vec2Vertex3D $ uhhCamPos uhh)
        (vec2Vertex3D $ vec0)
        (vec2Vector3D $ vecY)
    position (Light 0) $= vec2Vertex4 light0Pos 0
    render uhh
    flush
        
render uhh = do
    callList $ uhhDisplayList uhh
    
light0Pos = Vec3F 1 0 0

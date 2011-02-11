module Cube (renderCube) where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Utils

vertex3 (x, y, z) = vertex $ Vertex3 x y z

renderCube w = do
  renderPrimitive Quads $ mapM_ vertex3 (concat $ foo w)
    
foo w = [[
    ( w,  w,  w),
    ( w,  w, -w),
    ( w, -w, -w),
    ( w, -w,  w)],[
    (-w,  w,  w),
    (-w,  w, -w),
    (-w, -w, -w),
    (-w, -w,  w)],[
    ( w,  w,  w),
    ( w,  w, -w),
    (-w,  w, -w),
    (-w,  w,  w)],[
    ( w, -w,  w),
    ( w, -w, -w),
    (-w, -w, -w),
    (-w, -w,  w)],[
    ( w,  w,  w),
    ( w, -w,  w),
    (-w, -w,  w),
    (-w,  w,  w)],[
    ( w,  w, -w),
    ( w, -w, -w),
    (-w, -w, -w),
    (-w,  w, -w)]]


module Cube where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Debug.Trace

import Utils

data Side = Front | Back | Left | Right | Top | Bottom deriving Show
allSides = [
    Cube.Front, Cube.Back, Cube.Left,
    Cube.Right, Cube.Top, Cube.Bottom]

vertex3 (x, y, z) = vertex $ Vertex3 x y z

renderQuads size [] = renderQuads size allSides
renderQuads size visible = do
    renderPrimitive Quads $ quads w visible
    where
        w = size * 0.5
  
quads :: Double -> [Side] -> IO ()
quads w visible = do
    mapM_ vertex3 (concat $ map (flip cubeSide w) visible)

cubeSide = cs
cs Cube.Front w = [
    ( w,  w,  w),
    ( w, -w,  w),
    (-w, -w,  w),
    (-w,  w,  w)]
cs Cube.Back w = [
    ( w,  w, -w),
    ( w, -w, -w),
    (-w, -w, -w),
    (-w,  w, -w)]
cs Cube.Right w = [
    ( w,  w,  w),
    ( w,  w, -w),
    ( w, -w, -w),
    ( w, -w,  w)]
cs Cube.Left w = [
    (-w,  w,  w),
    (-w,  w, -w),
    (-w, -w, -w),
    (-w, -w,  w)]
cs Cube.Top w = [
    ( w,  w,  w),
    ( w,  w, -w),
    (-w,  w, -w),
    (-w,  w,  w)]
cs Cube.Bottom w = [
    ( w, -w,  w),
    ( w, -w, -w),
    (-w, -w, -w),
    (-w, -w,  w)]


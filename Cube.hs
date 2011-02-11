module Cube where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Debug.Trace

import Utils

data Side = Front | Back | Left | Right | Top | Bottom deriving Show
allSides = [
    Cube.Front, Cube.Back, Cube.Left,
    Cube.Right, Cube.Top, Cube.Bottom]
  
cubeQuads size visible = do
    map vertex3 (concat $ map (flip cubeSide w) visible)
    where
        w = size * 0.5
        vertex3 (x, y, z) = Vertex3 x y z

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


module Renderer where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Data.Vec.Packed
import qualified Data.Vec as Vec
import Vec

import Utils
import Voxelizer
import Cube

renderVoxels funny steps wireframe = do
    putStrLn $ "steps: " ++ (show steps)
    putStrLn $ "voxels: " ++ (show $ length voxels)
    mapM_ (flip renderVoxel wireframe) voxels
    where
        voxels = voxelize funny steps

renderVoxel (Voxel v@(Vec3D x y z) size visible) wireframe =
    preservingMatrix $ do
    translate $ vec2Vector3 v
    color $ vec2Color4 clr 1
    renderPrimitive Quads vertices
    iff wireframe $ do
        color $ Color3 (0::GLfloat) 0 0
        renderObject Wireframe (Cube size)
    where
        vertices = mapM_ vertex quads
        quads = cubeQuads size visible
        r = sin (Vec.norm v * 0.4)
        c = 0.05 + 0.9 * (Vec.sum d / 3)
        d = (Vec.map (\e -> 0.5 + 0.5 * sin e) (v * v))
        clr = Vec3D c c c


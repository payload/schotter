module Renderer where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Data.Vec.Packed
import qualified Data.Vec as Vec
import Vec

import Clean

renderBlock (Blocky pos) = renderBlock $ BlockyColored pos (Vec4F 1 1 1 1)
renderBlock (BlockyColored v (Vec4F r g b a)) = preservingMatrix $ do
    translate $ vec2Vector3 $ Vec.map fromIntegral v
    color $ Color4 r g b a
    renderObject Solid $ Cube 1
--    color $ Color3 (0::GLfloat) 0 0
--    renderObject Wireframe $ Cube 1.01

renderBlocks blocks@[BlockyColored _ _] = mapM_ renderBlock blocks
renderBlocks blocks@[Blocky _] = mapM_ renderBlock (blocksColorize blocks colors)
    where
        colors = cycle [
            Vec4F 0 0.5 0 1
            ]

renderBlocksOfBlocks bofb = mapM_ renderBlocks bofb

{-
renderVoxels funny step wireframe = do
    putStrLn $ "step: " ++ (show step)
    putStrLn $ "voxels: " ++ (show $ length voxels)
    mapM_ (flip renderVoxel wireframe) voxels
    where
        voxels = voxelize funny step

renderVoxel (Voxel v@(Vec3F x y z) size visible clr norm) wireframe =
    preservingMatrix $ do
    translate $ vec2Vector3 v
    materialAmbient FrontAndBack $= vec2Color4F clr 1
    materialSpecular FrontAndBack $= vec2Color4F clr 1
    normal $ vec2Normal3 norm
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
-}

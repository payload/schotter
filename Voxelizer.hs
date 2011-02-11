module Voxelizer where
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GLU
import Graphics.UI.GLUT
import GHC.Float
import Data.IORef

import Debug.Trace

import Utils
import Vec hiding (abs, map)
import qualified Vec
import Cube

-- Voxel
data Voxel = Voxel {
    voxPos :: Vec,
    voxSize :: Double,
    voxVisibleSides :: [Side]
} deriving Show
-- Voxel

renderVoxels r steps wireframe = do
    putStrLn $ "steps: " ++ (show steps)
    putStrLn $ "voxels: " ++ (show $ length voxels)
    mapM_ (\vxl -> renderVoxel vxl wireframe) voxels
    where
        voxels = voxelize r steps

renderVoxel (Voxel v size visible) wireframe =
    preservingMatrix $ do
    translate $ vec2Vector3 v
    color $ Color4 cx cy cz 1
    renderPrimitive Quads $ mapM_ vertex $ cubeQuads size visible
    iff wireframe $ do
        color $ Color3 (0::GLfloat) 0 0
        renderObject Wireframe (Cube size)
    where
        vv = Vec.map (\e -> 0.6 + (0.4 * sin e)) v
        cx = vv!0
        cy = vv!1
        cz = vv!2

voxelize r steps =
    trace ("step: " ++ show step)
    [Voxel v step s | (v,s) <- zip grid visibleSides, length s > 0]
    where
        visibleSides = map (voxelVisibleSides step (inside r)) grid
        step = (r*2) / int2Double steps
        grid = concat $ map
            (\x -> concat $ map
            (\y -> map
            (\z -> vec x y z) allSteps) allSteps) allSteps
        allSteps = take (steps+1) (iterate (\x -> x+step) (-r))

inside r v = (Vec.abs v) < r

voxelVisibleSides step inside v
    | inside v = voxelNeighborsInside step inside v
    | otherwise = []

voxelNeighborsInside step inside v =
    [s | (s,v) <- zip sides neighborsInside, not v]
    where
        neighborsInside = map inside $ map (add v) neighbors
        sides = [
            Cube.Right, Cube.Top, Cube.Front,
            Cube.Left, Cube.Bottom, Cube.Back]
        neighbors = vecs ++ map neg vecs
        vecs = map (mul step) [vecX, vecY, vecZ]

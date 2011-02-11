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

-- Voxel
data Voxel = Voxel {
    voxPos :: Vec,
    voxSize :: Double
} deriving Show
-- Voxel

renderVoxels r steps wireframe = do
    putStrLn $ "steps: " ++ (show steps)
    putStrLn $ "voxels: " ++ (show $ length voxels)
    --putStrLn $ "voxels: " ++ (show $ map (vec2List . voxPos) voxels)
    mapM_ (\vxl -> renderVoxel vxl wireframe) voxels
    where
        voxels = voxelize r steps

renderVoxel (Voxel v size) wireframe =
    preservingMatrix $ do
    translate $ vec2Vector3 v
    color $ Color4 cx cy cz 1
    renderObject Solid (Cube size)
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
    map (\v -> Voxel v step) $
    filter myFilter grid
    where
        myFilter = voxelFilter step $ inside r
        step = (r*2) / int2Double steps
        grid = concat $ map
            (\x -> concat $ map
            (\y -> map
            (\z -> vec x y z) allSteps) allSteps) allSteps
        allSteps = take (steps+1) (iterate (\x -> x+step) (-r))

inside r v = (Vec.abs v) < r

voxelFilter step inside v
    | inside v = voxelCheckNeighbors step inside v
    | otherwise = False

voxelCheckNeighbors step inside v =
    not $ all inside $ map (add v) neighbors
    where
        neighbors = vecs ++ map neg vecs
        vecs = map (mul step) [vecX, vecY, vecZ]

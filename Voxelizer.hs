module Voxelizer where
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GLU
import Graphics.UI.GLUT
import GHC.Float
import Data.IORef

import Utils

renderVoxels r steps = do
    putStrLn $ show $ length voxels
    mapM_ renderVoxel voxels
    where
        voxels = voxelize r steps

renderVoxel (x, y, z, s) =
    preservingMatrix $ do
    translate $ Vector3 x y z
    color $ Color3 clr clr clr
    renderObject Solid (Cube size)
    color $ Color3 (0::GLfloat) 0 0
    renderObject Wireframe (Cube size)
    where
        size = float2Double s
        clr = (2 + y)/3

voxelize r steps =
    map (\(x,y,z) -> (x,y,z,step)) $
    filter myFilter (grid (-r) r step)
    where
        step = ((r*2) / (steps2-1))
        steps2 = max 2 $ int2Float steps
        myFilter = voxelFilter $ inside r

voxelFilter inside c@(x, y, z) =
    inside c

grid begin end step =
    flatten $
    map (\x -> flatten $ map
        (\y -> map
        (\z -> (x, y, z)) allSteps) allSteps) allSteps
    where 
        allSteps = goThrough begin end step

goThrough begin end step =
    take (floor steps) (iterate (\x -> x+step) begin)
    where
        steps = (end - begin) / step
        
inside r (x, y, z) = (sqrt $ x*x + y*y + z*z) <= r

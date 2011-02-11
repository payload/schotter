module Voxelizer where
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GLU
import Graphics.UI.GLUT
import GHC.Float
import Data.IORef

import Utils

renderVoxels r steps wireframe = do
    putStrLn $ show $ length voxels
    mapM_ (\vxl -> renderVoxel vxl wireframe) voxels
    where
        voxels = voxelize r steps

renderVoxel (x, y, z, s) wireframe =
    preservingMatrix $ do
    translate $ Vector3 x y z
    color $ Color4 cx cy cz 1
    renderObject Solid (Cube size)
    renderVoxelWireCube size wireframe
    where
        size = float2Double s
        cx = abs x
        cy = abs y
        cz = abs z

renderVoxelWireCube size True = do
    color $ Color3 (0::GLfloat) 0 0
    renderObject Wireframe (Cube size)
renderVoxelWireCube _ False = return ()

voxelize r steps =
    map (\(x,y,z) -> (x,y,z,step)) $
    filter myFilter (grid (-r) r step)
    where
        step = ((r*2) / (steps2-1))
        steps2 = max 2 $ int2Float steps
        myFilter = voxelFilter $ inside r

voxelFilter inside c
    | inside c = voxelCheckNeighbors inside c
    | otherwise = False

voxelCheckNeighbors inside (x,y,z) =
    not $ all inside $ map addxyz neighbors
    where
        addxyz = \(nx,ny,nz) -> (nx+x, ny+y, nz+z)
        neighbors = [
            (-1,0,0),
            ( 1,0,0),
            (0,-1,0),
            (0, 1,0),
            (0,0,-1),
            (0,0, 1)]

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

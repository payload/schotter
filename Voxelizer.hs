module Voxelizer where
import GHC.Float

import Data.Vec.Packed
import qualified Data.Vec as Vec
import Vec

import Cube
import Funny

-- Voxel
data Voxel = Voxel {
    voxPos :: Vec3D,
    voxSize :: Double,
    voxVisibleSides :: [Side]
} deriving Show
-- Voxel

voxelize funny@(Funny inside bounds r) step =
    [Voxel v step s | (v,s) <- zip grid visibleSides, length s > 0]
    where
        visibleSides = map (voxelVisibleSides step funny) grid
        steps = double2Int $ (r*2) / step
        grid = voxelGrid allSteps
        allSteps = voxelStepThrough steps step r

voxelGrid allSteps =
    concat $ map
    (\x -> concat $ map
    (\y -> map
    (\z -> Vec3D x y z) allSteps) allSteps) allSteps

voxelStepThrough steps step r = 
    take (steps+1) (iterate (\x -> x+step) (-r))

voxelVisibleSides step funny@(Funny inside _ _) v
    | inside v = voxelNeighborsInside step funny v
    | otherwise = []

voxelNeighborsInside step (Funny inside bounds _) v =
    [s | (s,v) <- zip sides neighborsInside, not v]
    where
        neighborsInside = map (\e -> inside e && bounds e) neighbors
        neighbors = map (+ v) (vecs ++ map negate vecs)
        vecs = map (Vec.map (step *)) [vecX, vecY, vecZ]
        sides = [
            Cube.Right, Cube.Top, Cube.Front,
            Cube.Left, Cube.Bottom, Cube.Back]

module Voxelizer where
import GHC.Float

import Data.Vec.Packed
import qualified Data.Vec as Vec
import Vec

import Cube

-- Voxel
data Voxel = Voxel {
    voxPos :: Vec3D,
    voxSize :: Double,
    voxVisibleSides :: [Side]
} deriving Show
-- Voxel

voxelize r steps =
    [Voxel v step s | (v,s) <- zip grid visibleSides, length s > 0]
    where
        visibleSides = map (voxelVisibleSides step (inside r)) grid
        step = (r*2) / int2Double steps
        grid = voxelGrid allSteps
        allSteps = voxelStepThrough steps step r

voxelGrid allSteps =
    concat $ map
    (\x -> concat $ map
    (\y -> map
    (\z -> Vec3D x y z) allSteps) allSteps) allSteps

voxelStepThrough steps step r = 
    take (steps+1) (iterate (\x -> x+step) (-r))

inside r v@(Vec3D x y z) =
    ((Vec.dot v v) < (r*r)) && (not $ (x < 0) && (y > 0) && (z < 0))

voxelVisibleSides step inside v
    | inside v = voxelNeighborsInside step inside v
    | otherwise = []

voxelNeighborsInside :: Double -> (Vec3D -> Bool) -> Vec3D -> [Cube.Side]
voxelNeighborsInside step inside v =
    [s | (s,v) <- zip sides neighborsInside, not v]
    where
        neighborsInside = map inside $ map (+ v) neighbors
        neighbors = vecs ++ map negate vecs
        vecs = map (Vec.map (step *)) [vecX, vecY, vecZ]
        sides = [
            Cube.Right, Cube.Top, Cube.Front,
            Cube.Left, Cube.Bottom, Cube.Back]

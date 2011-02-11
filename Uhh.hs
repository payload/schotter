module Uhh where
import Graphics.Rendering.OpenGL
import Data.IORef

import Voxelizer
import Vec

-- Uhh
data Uhh = Uhh {
    uhhCamPos :: Vec,
    uhhLookAt :: Vec,
    uhhSteps  :: Int,
    uhhDisplayList :: DisplayList,
    uhhWireframe :: Bool
} deriving Show

newUhh campos lookat steps = do
    dl <- defineNewList Compile $
        renderVoxels 3 steps wireframe
    return (Uhh campos lookat steps dl wireframe)
    where
        wireframe = True
    
updateUhh uhhref = do
    uhh <- readIORef uhhref
    defineList (uhhDisplayList uhh) Compile $ do
        renderVoxels 3 (uhhSteps uhh) (uhhWireframe uhh)
-- Uhh

-- Voxel
data Voxel = Voxel {
    voxPos :: Vec
}
-- Voxel

module Uhh where
import Graphics.Rendering.OpenGL
import Data.IORef

import Data.Vec.Packed
import qualified Data.Vec as Vec
import Vec

import Voxelizer
import Renderer

-- Uhh
data Uhh = Uhh {
    uhhCamPos :: Vec3D,
    uhhLookAt :: Vec3D,
    uhhSteps  :: Int,
    uhhDisplayList :: DisplayList,
    uhhWireframe :: Bool
} deriving Show

newUhh campos lookat steps = do
    dl <- defineNewList Compile $
        renderVoxels 3 steps wireframe
    return (Uhh campos lookat steps dl wireframe)
    where
        wireframe = False
    
updateUhh uhhref = do
    uhh <- readIORef uhhref
    defineList (uhhDisplayList uhh) Compile $ do
        renderVoxels 3 (uhhSteps uhh) (uhhWireframe uhh)
-- Uhh

module Uhh where
import Graphics.Rendering.OpenGL
import Data.IORef

import Data.Vec.Packed
import qualified Data.Vec as Vec
import Vec

import Voxelizer
import Renderer
import Funny

-- Uhh
data Uhh = Uhh {
    uhhCamPos :: Vec3D,
    uhhLookAt :: Vec3D,
    uhhStep  :: Double,
    uhhDisplayList :: DisplayList,
    uhhWireframe :: Bool,
    uhhFunnyIndex :: Int
}

uhhFunny uhh = uhhFunnies !! uhhFunnyIndex uhh

uhhFunnies = funnies

modifyUhh uhhref f = do
    modifyIORef uhhref f
    updateUhh uhhref

newUhh campos lookat step = do
    dl <- defineNewList Compile $
        renderVoxels (uhhFunnies !! funny) step wireframe
    return (Uhh campos lookat step dl wireframe funny)
    where
        wireframe = False
        funny = 0
    
updateUhh uhhref = do
    uhh <- readIORef uhhref
    defineList (uhhDisplayList uhh) Compile $ do
        renderVoxels (uhhFunny uhh) (uhhStep uhh) (uhhWireframe uhh)
-- Uhh

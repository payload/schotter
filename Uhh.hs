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
    uhhSteps  :: Int,
    uhhDisplayList :: DisplayList,
    uhhWireframe :: Bool,
    uhhFunnyIndex :: Int
}

uhhFunny uhh = uhhFunnies !! uhhFunnyIndex uhh

uhhFunnies = [funnyFun 3, funnySphere 3]

modifyUhh uhhref f = do
    modifyIORef uhhref f
    updateUhh uhhref

newUhh campos lookat steps = do
    dl <- defineNewList Compile $
        renderVoxels (uhhFunnies !! funny) steps wireframe
    return (Uhh campos lookat steps dl wireframe funny)
    where
        wireframe = False
        funny = 0
    
updateUhh uhhref = do
    uhh <- readIORef uhhref
    defineList (uhhDisplayList uhh) Compile $ do
        renderVoxels (uhhFunny uhh) (uhhSteps uhh) (uhhWireframe uhh)
-- Uhh

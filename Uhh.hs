module Uhh where
import Graphics.Rendering.OpenGL
import Data.IORef

import Voxelizer

data Vec = Vec {
    vecX :: Double,
    vecY :: Double,
    vecZ :: Double
} deriving Show

makeVec x y z = Vec x y z

vecLeft  (Vec x y z) = Vec (x-1) y z
vecRight (Vec x y z) = Vec (x+1) y z
vecUp    (Vec x y z) = Vec x (y-1) z
vecDown  (Vec x y z) = Vec x (y+1) z
vecForward (Vec x y z) = Vec x y (z+1)
vecBackward (Vec x y z) = Vec x y (z-1)

vec2Vertex3 (Vec x y z) = Vertex3 x y z

data Uhh = Uhh {
    uhhCamPos :: Vec,
    uhhLookAt :: Vec,
    uhhSteps  :: Int,
    uhhDisplayList :: DisplayList
} deriving Show

newUhh campos lookat steps = do
    dl <- defineNewList Compile $
        renderVoxels 3 steps
    return (Uhh campos lookat steps dl)
    
updateUhh uhhref = do
    uhh <- readIORef uhhref
    defineList (uhhDisplayList uhh) Compile $ do
        renderVoxels 3 (uhhSteps uhh)

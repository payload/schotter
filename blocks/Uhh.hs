module Uhh where
import Graphics.Rendering.OpenGL
import Data.IORef
import Data.Vec.Packed

import Renderer
import Clean
import UnsaneDefaults

-- Uhh
data Uhh = Uhh {
    uhhCamPos :: Vec3F,
    uhhLookAt :: Vec3F,
    uhhDisplayList :: DisplayList,
    uhhWireframe :: Bool,
    uhhÜhh :: Ühh
}

uhhSimulate dt uhh =
    uhh { uhhÜhh = ühhSimulate dt $ uhhÜhh uhh }

uhhRender uhh =
    renderBlocksOfBlocks $ ühhBlocks $ uhhÜhh uhh

modifyUhh uhhref f = do
    modifyIORef uhhref f
    updateUhh uhhref

newUhh campos lookat = do
    dl <- defineNewList Compile $ do return ()
    return (Uhh campos lookat dl wireframe ühh)
    where
        wireframe = False
        ühh = ühhDefault
    
updateUhh uhhref = do
    uhh <- readIORef uhhref
    defineList (uhhDisplayList uhh) Compile $ do uhhRender uhh
-- Uhh

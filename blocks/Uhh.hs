module Uhh where
import Graphics.Rendering.OpenGL
import Data.IORef

import Data.Vec.Packed
import qualified Data.Vec as Vec
import Vec

import Renderer
import Simulator

-- Uhh
data Uhh = Uhh {
    uhhCamPos :: Vec3F,
    uhhLookAt :: Vec3F,
    uhhDisplayList :: DisplayList,
    uhhWireframe :: Bool,
    uhhSimStuff :: [SimObj]
}

uhhDefaultRender uhh =
    renderBlocks $ map simPos (uhhSimStuff uhh)

uhhDefaultSimStuff = 
    concat
    [[newSimSinus (Vec3F (-10 + x) 0 (-10 + y)) (Vec3F 0 1 0) 1 (x+y)
        | x <- take (floor n) $ iterate (1 +) 0]
        | y <- take (floor n) $ iterate (1 +) 0]
    where
        n = 20

modifyUhh uhhref f = do
    modifyIORef uhhref f
    updateUhh uhhref

newUhh campos lookat = do
    dl <- defineNewList Compile $ do return ()
    return (Uhh campos lookat dl wireframe uhhDefaultSimStuff)
    where
        wireframe = False
    
updateUhh uhhref = do
    uhh <- readIORef uhhref
    defineList (uhhDisplayList uhh) Compile $ do uhhDefaultRender uhh
-- Uhh

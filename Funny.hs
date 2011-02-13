module Funny where
import Data.Vec.Packed
import qualified Data.Vec as Vec
import Vec

data Funny = Funny {
    funnyInside :: Vec3D -> Bool,
    funnyR :: Double
}

funnies = [funnyFun 3, funnySphere 3 vec0, funnySphere 3 vecX]

funnySphere r pos =
    Funny inside r
    where
        inside v = Vec.normSq (pos-v) < r*r
        
funnyFun r =
    Funny inside r
    where
        inside v@(Vec3D x y z) =
            Vec.normSq v < r*r && (not $ x < 0 && y > 0 && z < 0)
            
module Funny where
import Data.Vec.Packed
import qualified Data.Vec as Vec
import Vec

data Funny = Funny {
    funnyInside :: Vec3D -> Bool,
    funnyBounds :: Vec3D -> Bool,
    funnyR :: Double
}

funnies = [funnyWave 10, funnyDodo 3, funnySphere 3 vec0, funnyFun 3, funnySphere 3 vecX]

bounds r v =
    all between (Vec.toList v)
    where
        between x = -r < x && x < r

funnySphere r pos = Funny (insideSphere r pos) (bounds r) r
insideSphere r pos v = Vec.normSq (pos-v) < r*r
        
funnyDodo r = Funny inside (bounds r) r
    where
        inside v =
            Vec.normSq ((Vec3D (-r*0.66) 0 0) - v) < r*r &&
            (not $ Vec.normSq v < r*r)
            
funnyWave r = Funny inside (bounds r) r
    where
        inside v@(Vec3D x y z) =
            z < x*y
        
funnyFun r =
    Funny inside (bounds r) r
    where
        inside v@(Vec3D x y z) =
            Vec.normSq v < r*r && (not $ x < 0 && y > 0 && z < 0)
            

module Funny where
import Data.Vec.Packed
import qualified Data.Vec as Vec
import Vec

data Funny = Funny {
    funnyInside :: Vec3D -> Bool,
    funnyBounds :: Vec3D -> Bool,
    funnyNormal :: Vec3D -> Vec3D,
    funnyColor :: Vec3D -> Vec3D,
    funnyR :: Double
}

funnies = [funnySphere 3 vec0]--[funnyWave 10, funnyDodo 3, funnySphere 3 vec0, funnyFun 3, funnySphere 3 vecX]

bounds r v =
    all between (Vec.toList v)
    where
        between x = -r < x && x < r

funnySphere r pos =
    Funny
    (insideSphere r pos)
    (bounds r)
    (normalSphere pos)
    (colorSphere r pos)
    r

insideSphere r pos v = Vec.normSq (pos-v) < r*r
normalSphere pos v = Vec.normalize (pos-v)
colorSphere r pos@(Vec3D px py pz) v@(Vec3D x y z) =
    Vec3D cr cg cb
    where
        cr = 0.1 + 0.9*((x-px+r)/r/2)
        cg = cr
        cb = cr
        
{-
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
-}

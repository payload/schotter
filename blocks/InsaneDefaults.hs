module InsaneDefaults where
import GHC.Float
import Vec
import Data.Vec.Packed
import qualified Data.IntMap as IntMap
import Clean

ühhDefault =
    insaneWorld $ Ühh IntMap.empty []

insaneWorld ühh =
    insaneStuff
    (ühh { ühhSimmies = simmies, ühhFunnies = funnies })
    where
        s = 10
        groundColors = cycle [Vec4F 0 0.3 0 1, Vec4F 0 0.28 0 1]
        groundTopLeft = Vec3I (-s) 0 (-s)
        groundBottomRight = Vec3I (1+s) 1 (1+s)
        ground = insaneBox groundTopLeft groundBottomRight groundColors
        simmies = ühhSimmies ühh
        funnies = ground ++ ühhFunnies ühh
        
insaneStuff ühh =
    ühh {
        ühhSimmies = IntMap.union curSimmies simmies,
        ühhFunnies = (ühhFunnies ühh) ++ funnies
    }
    where
        curSimmies = ühhSimmies ühh
        maxkey im | IntMap.null im = 0
        maxkey im | otherwise = 1 + (last $ IntMap.keys im)
        idstart = maxkey curSimmies
        ids = iterate (1+) idstart
        simmies = IntMap.fromList $ zip ids [Simmy vecY vec0]
        funnies = map Funny $ IntMap.keys simmies
        
insaneBox (Vec3I ax ay az) (Vec3I bx by bz) colors =
    zipWith FunnyStatic posis colors
    where
        dx = bx - ax
        dy = by - ay
        dz = bz - az
        straight start amount = take amount $ iterate (1+) start
        xs = straight ax dx
        ys = straight ay dy
        zs = straight az dz
        posis = [Vec3I x y z | x <- xs, y <- ys, z <- zs]

defaultSimmies = 
    concat [circleSimmies (r * 2 * vecY) r 10 | r <- rs]
    where
        rs = take 10 $ iterate (1 +) 1

circleSimmies pos r steps =
    map (\v -> Simmy (pos + r * v) vec0) (circle steps)
    
circle steps =
    map xyz2vec (zip3 xs ys zs)
    where
        next = (pi*2/(int2Float steps) +)
        as = iterate next 0
        bs = take steps as
        xs = map cos bs
        ys = repeat 0
        zs = map sin bs

defaultFunnies simmies = 
    zipWith FunnyLine sima simb
    where 
        sima = take (length simmies) (iterate (1 +) 0)
        simb = tail sima ++ [head sima]

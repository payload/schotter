module Clean where
import Data.Vec.Packed
import qualified Data.Vec as Vec
import Data.Graph
import Data.List

data Ühh =
    Ühh {
        ühhSimmies :: [Simmy],
        ühhFunnies :: [Funny]
    }

ühhSimulate dt ühh = 
    ühh { ühhSimmies = map (simmySimulate dt) (ühhSimmies ühh) }

ühhBlocks ühh = map (funnyBlocks (ühhSimmies ühh)) (ühhFunnies ühh)

data Blocky =
    Blocky {
        blockPos :: Vec3I
    } |
    BlockyColored {
        blockPos :: Vec3I,
        blockColor :: Vec4F
    }

blocksColorize blocks colors =
    zipWith BlockyColored posis colors
    where
        posis = map blockPos blocks

data Simmy =
    Simmy {
        simPos :: Vec3F,
        simVel :: Vec3F
    }

simmySimulate dt simmy = simmy { simPos = simPos simmy + dt * simVel simmy }

data Funny =
    Funny {
        funSim :: Vertex
    } |
    FunnyLine {
        funA :: Vertex,
        funB :: Vertex
    }
    
vecF2I (Vec3F x y z) = Vec3I (round x) (round y) (round z)
    
funnyBlocks simmies (Funny sim) = [Blocky $ vecF2I $ simPos $ simmies !! sim]
funnyBlocks simmies (FunnyLine a b) = 
    map Blocky (rasterizeLine apos bpos)
    where
        apos = simPos $ simmies !! a
        bpos = simPos $ simmies !! b

rasterizeLine a b =
    nub ivecs
    where
        diff = (b - a)
        dist = Vec.norm diff 
        n = Vec.normalize diff
        vecs = take (round dist) (iterate (n +) a)
        ivecs = map vecF2I vecs

{-
d = 2×Δy – Δx
ΔO = 2×Δy
ΔNO = 2×(Δy − Δx)
y = ya
Pixel (xa, ya) einfärben
Für jedes x von xa+1 bis xe
    Wenn d ≤ 0
        d = d + ΔO
    ansonsten
        d = d + ΔNO
        y = y + 1
    Pixel (x, y) einfärben
-}

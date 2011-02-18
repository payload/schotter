module Clean where
import Data.Vec.Packed
import qualified Data.Vec as Vec
import Data.Graph
import Data.List
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

data Ühh =
    Ühh {
        ühhSimmies :: IntMap Simmy,
        ühhFunnies :: [Funny]
    }

ühhSimulate dt ühh = 
    ühh { ühhSimmies = IntMap.map (simmySimulate dt) (ühhSimmies ühh) }

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
    } |
    FunnyStatic {
        funPos :: Vec3I,
        funColor :: Vec4F
    }
    
vecF2I (Vec3F x y z) = Vec3I (round x) (round y) (round z)
    
funnyBlocks simmies (Funny sim) = [Blocky $ vecF2I $ simPos $ simmies IntMap.! sim]
funnyBlocks simmies (FunnyLine a b) = 
    map Blocky (rasterizeLine apos bpos)
    where
        apos = simPos $ simmies IntMap.! a
        bpos = simPos $ simmies IntMap.! b
funnyBlocks _ (FunnyStatic pos clr) = [BlockyColored pos clr]

rasterizeLine a b =
    nub ivecs
    where
        diff = (b - a)
        dist = Vec.norm diff 
        n = Vec.normalize diff
        vecs = take (ceiling dist) (iterate (n +) a)
        ivecs = map vecF2I vecs


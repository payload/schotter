module Simulator where

import Data.Vec.Packed
import qualified Data.Vec as Vec
import Vec

import Debug.Trace

data SimObj =
    SimObj {
        simPos :: Vec3F,
        simAni :: (SimObj -> SimObj)
    } |
    SimObjTime {
        simPos :: Vec3F,
        simAni :: (SimObj -> SimObj),
        simTime :: Float
    }

newSimObj pos ani = SimObj pos ani
newSimSinus pos max time offset = SimObjTime pos (aniSinus pos max time) offset

simulate [] = []
simulate (s:t) = (simAni s) s : simulate t

aniLinearMove v s = s { simPos = simPos s + v }

aniSinus pos max time s =
    s {
        simPos = pos + Vec.map ((sin simtime) *) max,
        simTime = simtime + 1/(time * 3.14159 * 10)
    }
    where
        simtime = simTime s

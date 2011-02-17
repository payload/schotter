module Simulator where

import Data.Vec.Packed
import qualified Data.Vec as Vec
import Vec

import Debug.Trace

data SimObj =
    SimObj {
        simPos :: Vec3F,
        simAni :: (Float -> SimObj -> SimObj)
    } |
    SimObjTime {
        simPos :: Vec3F,
        simAni :: (Float -> SimObj -> SimObj),
        simTime :: Float
    }

newSimObj pos = SimObj pos (\dt s -> s)
newSimLinearMove pos vel = SimObj pos $ aniLinearMove vel
newSimSinus pos max time offset = SimObjTime pos (aniSinus pos max time) offset

simulateStuff dt stuff = map (simulate dt) stuff

simulate dt s = (simAni s) dt s

aniLinearMove v dt s = s { simPos = simPos s + Vec.map (dt*) v }

aniSinus pos max time dt s =
    s {
        simPos = pos + Vec.map ((sin simtime) *) max,
        simTime = simtime + 1/(time * 3.14159)
    }
    where
        simtime = simTime s

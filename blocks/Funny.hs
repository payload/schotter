module Funny where
import Simulator
import Data.Graph

data Funny =
    Funny {
        funSim :: Vertex
    } |
    FunnyLine {
        funSim :: Vertex,
        funOther :: Vertex
    }
    
--funPos fun = simPos $ funSim fun

funBlockPosis sims (Funny sim) = [simPos $ sims !! sim]
funBlockPosis sims (FunnyLine a b) =
    [apos, midpos, bpos]
    where
        apos = simPos $ sims !! a
        bpos = simPos $ sims !! b
        midpos = apos + 0.5 * (bpos - apos)


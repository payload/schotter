module Funny where
import Simulator

data Funny =
    Funny {
        funSim :: SimObj
    } |
    FunnyLink {
        funSim :: SimObj,
        funLink :: Funny
    }
    
funPos fun = simPos $ funSim fun
    
funSimulate dt (Funny sim) = Funny (simulate dt sim)
funSimulate dt (FunnyLink sim link) = FunnyLink (simulate dt sim) link

newFunLinearMove pos vel = Funny $ newSimLinearMove pos vel

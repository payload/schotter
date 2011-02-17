module UnsaneDefaults where
import GHC.Float
import Vec
import Clean

import Debug.Trace

ühhDefault = Ühh simmies funnies
    where
        simmies = defaultSimmies
        funnies = defaultFunnies simmies

defaultSimmies =
    map (Simmy vec0 . (5 *)) vels
    where
        steps = 4
        next = (pi*2/(int2Float steps) +)
        as = iterate next 0
        bs = take steps as
        xs = map cos bs
        ys = map (\x->0) bs
        zs = map sin bs
        vels = map xyz2vec (zip3 xs ys zs)

defaultFunnies simmies = 
    zipWith FunnyLine sima simb
    where 
        sima = take (length simmies) (iterate (1 +) 0)
        simb = tail sima ++ [head sima]

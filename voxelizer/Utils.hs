module Utils where

iff cond f | cond = f
           | otherwise = return ()

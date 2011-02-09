module Utils where

flatten :: [[a]] -> [a]
flatten l = foldl (++) [] l


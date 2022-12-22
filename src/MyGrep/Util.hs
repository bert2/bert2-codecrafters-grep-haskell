module MyGrep.Util where

sortPair :: Ord a => (a, a) -> (a, a)
sortPair (x, y) = if x <= y then (x, y) else (y, x)

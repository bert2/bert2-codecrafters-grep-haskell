module MyGrep.Util where

import Data.Char
import Numeric (showIntAtBase)

sortPair :: Ord a => (a, a) -> (a, a)
sortPair (x, y) | x > y = (y, x)
sortPair p              = p

charToHex :: Char -> String
charToHex c = showIntAtBase 16 (toUpper . intToDigit) (ord c) ""

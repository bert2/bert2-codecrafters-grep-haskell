module MyGrep.URI (encode) where

import Data.Char
import Numeric

encode :: String -> String
encode = mconcat . map encodeChar

encodeChar :: Char -> String
encodeChar c = if isAllowed c then [c] else charToHex c

isAllowed :: Char -> Bool
isAllowed c = isAlphaNum c || (c `elem` "-_.~")

charToHex :: Char -> String
charToHex c = "%" ++ showIntAtBase 16 (toUpper . intToDigit) (ord c) ""

module MyGrep.URI (encode) where

import Data.Char (isAlphaNum)
import MyGrep.Util (charToHex)

encode :: String -> String
encode = mconcat . map encodeChar

encodeChar :: Char -> String
encodeChar c = if isAllowed c then [c] else "%" ++ charToHex c

isAllowed :: Char -> Bool
isAllowed c = isAlphaNum c || (c `elem` "-_.~")

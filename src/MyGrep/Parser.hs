module MyGrep.Parser (parseRegex) where

import Data.Bifunctor
import Data.Functor ((<&>), ($>))
import Data.Maybe
import Data.Void (Void)
import MyGrep.NFA.Build qualified as NFA
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

parseRegex :: String -> Either String NFA.StateB
parseRegex = bimap errorBundlePretty mconcat . runParser regex ""

regex :: Parser [NFA.StateB]
regex = do
  start <- startAnchorOrAnyString
  pattern <- regex'
  end <- endAnchorOrAnyString <* eof
  return [start, pattern, end]

regex' :: Parser NFA.StateB
regex' = choice [
  wordCharClass    $> NFA.oneOf [NFA.charRange ('0', '9'),
                                 NFA.charRange ('A', 'Z'),
                                 NFA.charRange ('a', 'z'),
                                 NFA.literalChar '_'],
  digitCharClass   $> NFA.charRange ('0', '9'),
  escapedChar     <&> NFA.literalChar,
  literalChar     <&> NFA.literalChar]

startAnchorOrAnyString :: Parser NFA.StateB
startAnchorOrAnyString = maybe NFA.anyString (const mempty) <$> optional (char '^')

endAnchorOrAnyString :: Parser NFA.StateB
endAnchorOrAnyString = maybe NFA.anyString (const mempty) <$> optional (char '$')

digitCharClass :: Parser ()
digitCharClass = () <$ string "\\d"

wordCharClass :: Parser ()
wordCharClass = () <$ string "\\w"

literalChar :: Parser Char
literalChar = noneOf ['^', '$', '\\'] <?> "literal character"

escapedChar :: Parser Char
escapedChar = char '\\' *> oneOf "^$\\" <?> "'^', '$', or '\'"

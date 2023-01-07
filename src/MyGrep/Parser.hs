module MyGrep.Parser (parseRegex) where

import Control.Monad
import Data.Bifunctor
import Data.Functor ((<&>), ($>))
import Data.List
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
  wordCharClass   $> NFA.oneOf [NFA.charRange ('0', '9'),
                                NFA.charRange ('A', 'Z'),
                                NFA.charRange ('a', 'z'),
                                NFA.literalChar '_'],
  digitCharClass  $> NFA.charRange ('0', '9'),
  charClass      <&> NFA.charRange,
  litOrEscChar   <&> NFA.literalChar]

startAnchorOrAnyString :: Parser NFA.StateB
startAnchorOrAnyString = maybe NFA.anyString (const mempty) <$> optional (char '^')

endAnchorOrAnyString :: Parser NFA.StateB
endAnchorOrAnyString = maybe NFA.anyString (const mempty) <$> optional (char '$')

digitCharClass :: Parser ()
digitCharClass = () <$ string "\\d" <?> "digit character class"

wordCharClass :: Parser ()
wordCharClass = () <$ string "\\w" <?> "word character class"

charClass :: Parser (Char, Char)
charClass = between (char '[') (char ']') charRange
  where charRange = (,) <$> litOrEscChar <* char '-' <*> litOrEscChar
        litOrEscChar = charWithReserved "^-\\[]"

litOrEscChar :: Parser Char
litOrEscChar = charWithReserved "^$\\[]"

charWithReserved :: [Char] -> Parser Char
charWithReserved res = escChar <|> litChar
  where litChar = noneOf res <?> "any character (except " ++ resLbl ++ ")"
        escChar = char '\\' *> resChar <?> "escape sequence"
        resChar = oneOf res <?> resLbl
        resLbl = pprintChars res

pprintChars :: [Char] -> String
pprintChars chars = (mconcat . intersperse ", " . init) quoted ++ ", or " ++ last quoted
  where quoted = map (\c -> "'" ++ [c] ++ "'") chars

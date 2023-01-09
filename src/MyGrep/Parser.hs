module MyGrep.Parser (parseRegex) where

import Control.Monad
import Data.Bifunctor
import Data.Functor ((<&>), ($>))
import Data.List
import Data.Maybe
import Data.Void (Void)
import MyGrep.NFA.Base qualified as NFA
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
  negCharClass   <&> NFA.noneOf,
  charClass      <&> NFA.oneOf,
  litOrEscChar   <&> NFA.literalChar]

startAnchorOrAnyString :: Parser NFA.StateB
startAnchorOrAnyString = maybe NFA.anyString (const mempty) <$> optional (char '^')

endAnchorOrAnyString :: Parser NFA.StateB
endAnchorOrAnyString = maybe NFA.anyString (const mempty) <$> optional (char '$')

digitCharClass :: Parser ()
digitCharClass = () <$ string "\\d" <?> "digit character class"

wordCharClass :: Parser ()
wordCharClass = () <$ string "\\w" <?> "word character class"

negCharClass :: Parser [NFA.CharMatch]
negCharClass = between (string "[^") (char ']') (some singleOrRange)
  where singleOrRange = choice [singleChar <&> NFA.LiteralChar,
                                charRange  <&> NFA.CharRange]
        singleChar = try $ litOrEscChar <* notFollowedBy (char '-')
        charRange = (,) <$> litOrEscChar <* char '-' <*> litOrEscChar <?> "character range"
        litOrEscChar = charWithReserved "^$\\[]-"

charClass :: Parser [NFA.StateB]
charClass = between (char '[') (char ']') (some singleOrRange)
  where singleOrRange = choice [singleChar <&> NFA.literalChar,
                                charRange  <&> NFA.charRange]
        singleChar = try $ litOrEscChar <* notFollowedBy (char '-')
        charRange = (,) <$> litOrEscChar <* char '-' <*> litOrEscChar <?> "character range"
        litOrEscChar = charWithReserved "^$\\[]-"

litOrEscChar :: Parser Char
litOrEscChar = charWithReserved "^$\\[]"

charWithReserved :: [Char] -> Parser Char
charWithReserved res = escChar <|> litChar
  where litChar = noneOf res <?> "any character (except " ++ resLbl ++ ")"
        escChar = char '\\' *> resChar <?> "escape sequence"
        resChar = oneOf res <?> resLbl
        resLbl  = pprintChars res

pprintChars :: [Char] -> String
pprintChars chars = (mconcat . intersperse ", " . init) quoted ++ ", or " ++ last quoted
  where quoted = map (\c -> "'" ++ [c] ++ "'") chars

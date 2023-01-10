module MyGrep.Parser (parseRegex) where

import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Bifunctor
import Data.Functor ((<&>), ($>))
import Data.List
import Data.Maybe
import Data.Void (Void)
import MyGrep.NFA.Base qualified as NFA
import MyGrep.NFA.Build qualified as NFA
import MyGrep.Util (sortPair)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

parseRegex :: String -> Either String NFA.StateB
parseRegex = bimap errorBundlePretty mconcat . runParser regex ""

regex :: Parser [NFA.StateB]
regex = do
  start <- startAnchorOrAnyString
  pattern <- makeExprParser regex' opTbl
  end <- endAnchorOrAnyString <* eof
  return [start, pattern, end]

opTbl :: [[Operator Parser NFA.StateB]]
opTbl = [[Postfix (char '*' $> NFA.zeroOrMore),
          Postfix (char '+' $> NFA.oneOrMore),
          Postfix (char '?' $> NFA.zeroOrOne)],
         [InfixL  (char '|' $> NFA.alternation)]]

regex' :: Parser NFA.StateB
regex' = choice [
  wordCharClass   $> NFA.oneOf [NFA.charRange ('0', '9'),
                                NFA.charRange ('A', 'Z'),
                                NFA.charRange ('a', 'z'),
                                NFA.literalChar '_'],
  digitCharClass  $> NFA.charRange ('0', '9'),
  negCharClass   <&> NFA.noneOf,
  posCharClass   <&> NFA.oneOf,
  wildcard        $> NFA.anyChar,
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
negCharClass = charClass False NFA.LiteralChar (NFA.CharRange . sortPair) <?> "negative character class"

posCharClass :: Parser [NFA.StateB]
posCharClass = charClass True NFA.literalChar NFA.charRange <?> "positive character class"

charClass :: Bool -> (Char -> a) -> ((Char, Char) -> a) -> Parser [a]
charClass positive litf rangef = between open (char ']') (some singleOrRange)
  where open = if positive then string "[" else string "[^"
        singleOrRange = choice [singleChar <&> litf,
                                charRange  <&> rangef]
        singleChar = try $ litOrEscChar <* notFollowedBy (char '-')
        charRange = (,) <$> litOrEscChar <* char '-' <*> litOrEscChar <?> "character range"
        litOrEscChar = charWithReserved "^$\\[]-"

wildcard :: Parser ()
wildcard = () <$ char '.'

litOrEscChar :: Parser Char
litOrEscChar = charWithReserved "^$\\|*+?[]"

charWithReserved :: [Char] -> Parser Char
charWithReserved res = escChar <|> litChar
  where litChar = noneOf res <?> "any character (except " ++ resLbl ++ ")"
        escChar = char '\\' *> resChar <?> "escape sequence"
        resChar = oneOf res <?> resLbl
        resLbl  = pprintChars res

pprintChars :: [Char] -> String
pprintChars chars = (mconcat . intersperse ", " . init) quoted ++ ", or " ++ last quoted
  where quoted = map (\c -> ['\'', c, '\'']) chars

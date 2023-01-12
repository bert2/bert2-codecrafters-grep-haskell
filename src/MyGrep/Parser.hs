module MyGrep.Parser (parseRegex) where

import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Bifunctor
import Data.Functor ((<&>), ($>))
import Data.List (intersperse)
import Data.Maybe
import Data.Void (Void)
import MyGrep.NFA.Base qualified as NFA
import MyGrep.NFA.Build qualified as NFA
import MyGrep.Util (sortPair)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

parseRegex :: String -> Either String NFA.StateB
parseRegex = first errorBundlePretty . runParser regex' ""

regex' :: Parser NFA.StateB
regex' = do
  start <- optional (char '^') <&> maybe NFA.anyString (const mempty)
  inner <- optional regex      <&> fromMaybe mempty
  end   <- optional (char '$') <&> maybe NFA.anyString (const mempty)
  _     <- eof
  return $ start <> inner <> end

hiOpTbl :: [[Operator Parser NFA.StateB]]
hiOpTbl = [[Postfix (char '*' $> NFA.zeroOrMore),
            Postfix (char '+' $> NFA.oneOrMore),
            Postfix (char '?' $> NFA.zeroOrOne)]]

loOpTbl :: [[Operator Parser NFA.StateB]]
loOpTbl = [[InfixL (char '|' $> NFA.alternation)]]

implicitOp :: NFA.StateB -> NFA.StateB -> NFA.StateB
implicitOp = (<>)

regex :: Parser NFA.StateB
regex = makeExprParser' term hiOpTbl loOpTbl implicitOp

term :: Parser NFA.StateB -> Parser NFA.StateB
term term' = choice [
  group term'    <&> fromMaybe mempty,
  wordCharClass   $> NFA.oneOf [NFA.charRange ('0', '9'),
                                NFA.charRange ('A', 'Z'),
                                NFA.charRange ('a', 'z'),
                                NFA.literalChar '_'],
  digitCharClass  $> NFA.charRange ('0', '9'),
  negCharClass   <&> NFA.noneOf,
  posCharClass   <&> NFA.oneOf,
  wildcard        $> NFA.anyChar,
  litOrEscChar   <&> NFA.literalChar]

group :: Parser NFA.StateB -> Parser (Maybe NFA.StateB)
group term = between (char '(') (char ')') (optional term) <?> "match group"

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
wildcard = () <$ char '.' <?> "wildcard"

litOrEscChar :: Parser Char
litOrEscChar = charWithReserved "^$\\|*+?()[]"

charWithReserved :: [Char] -> Parser Char
charWithReserved res = escChar <|> litChar
  where litChar = noneOf res <?> "character literal"
        escChar = char '\\' *> resChar <?> "escape sequence"
        resChar = oneOf res <?> resLbl
        resLbl  = pprintChars res

pprintChars :: [Char] -> String
pprintChars chars = (mconcat . intersperse ", " . init) quoted ++ ", or " ++ last quoted
  where quoted = map (\c -> ['\'', c, '\'']) chars

-- creates an expr parser that understands implicit operators
makeExprParser' :: (Parser a -> Parser a)
                -> [[Operator Parser a]]
                -> [[Operator Parser a]]
                -> (a -> a -> a)
                -> Parser a
makeExprParser' termf hiOps loOps implicitf = lo
  where hi = makeExprParser hiTerm hiOps
        lo = makeExprParser loTerm loOps
        hiTerm = termf lo
        loTerm = some hi <&> foldr1 implicitf

module MyGrep.NFA.Base where

import Control.Monad (liftM2)
import Data.Hashable
import GHC.Arr (inRange)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (eqStableName, makeStableName)

data State = State {accepts :: Match, next :: State}
           | Split {left :: State, right :: State}
           | Final

data Match = AnyChar
           | PositiveMatch CharMatch
           | NegativeMatch [CharMatch]
           deriving Show

data CharMatch = LiteralChar Char
               | CharRange (Char, Char)
               deriving Show

instance Eq State where
  x == y = unsafePerformIO do
    liftM2 eqStableName (makeStableName x) (makeStableName y)

instance Hashable State where
  hashWithSalt salt state = unsafePerformIO do
    hashWithSalt salt <$> makeStableName state

matches :: Match -> Char -> Bool
matches AnyChar                          _ = True
matches (PositiveMatch (LiteralChar c')) c = c == c'
matches (PositiveMatch (CharRange r))    c = inRange r c
matches (NegativeMatch ms)               c = and $ map (notMatches c) ms
  where notMatches c m = not $ matches (PositiveMatch m) c

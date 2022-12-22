module MyGrep.NFA.Base where

import Control.Monad (liftM2)
import Data.Hashable
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (eqStableName, makeStableName)

data State = State {accepts :: Match, next :: State}
           | Split {left :: State, right :: State}
           | Final

data Match = AnyChar
           | LiteralChar Char
           | CharRange (Char, Char)
           deriving (Eq, Ord, Show)

instance Eq State where
  x == y = unsafePerformIO $
    liftM2 eqStableName (makeStableName x) (makeStableName y)

instance Hashable State where
  hashWithSalt salt state = unsafePerformIO $
    hashWithSalt salt <$> makeStableName state

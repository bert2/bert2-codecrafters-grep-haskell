{-# LANGUAGE DerivingVia #-}

module MyGrep.NFA.Build (
  StateB, buildNFA,
  anyChar, anyString, literalChar, charRange,
  alternation, oneOf, noneOf, zeroOrOne, zeroOrMore, oneOrMore
) where

import Data.Monoid
import MyGrep.NFA.Base
import MyGrep.Util (sortPair)
import Text.Show.Functions

newtype StateB = StateB {buildState :: State -> State}
                 deriving Show
                 deriving (Semigroup, Monoid) via Endo State

data LoopBehavior = OneOrMore | ZeroOrMore

buildNFA :: StateB -> State
buildNFA sb = buildState sb Final

anyChar :: StateB
anyChar = StateB $ State AnyChar

anyString :: StateB
anyString = StateB $ loop ZeroOrMore anyChar

literalChar :: Char -> StateB
literalChar = StateB . State . PositiveMatch . LiteralChar

charRange :: (Char, Char) -> StateB
charRange = StateB . State . PositiveMatch . CharRange . sortPair

alternation :: StateB -> StateB -> StateB
alternation l r = StateB $ branch l r

oneOf :: [StateB] -> StateB
oneOf = foldr1 alternation

noneOf :: [CharMatch] -> StateB
noneOf = StateB . State . NegativeMatch

zeroOrOne :: StateB -> StateB
zeroOrOne = StateB . branch mempty

zeroOrMore :: StateB -> StateB
zeroOrMore = StateB . loop ZeroOrMore

oneOrMore :: StateB -> StateB
oneOrMore = StateB . loop OneOrMore

branch :: StateB -> StateB -> State -> State
branch left right join = Split (buildState left join) (buildState right join)

loop :: LoopBehavior -> StateB -> State -> State
loop behavior body exit =
  case behavior of
    ZeroOrMore -> entry
    OneOrMore  -> body'
  where body' = buildState body entry
        entry = Split body' exit

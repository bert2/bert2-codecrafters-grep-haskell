{-# LANGUAGE DerivingVia #-}

module MyGrep.NFA.Build (
  StateB, buildNFA,
  anyChar, anyString, literalChar, charRange,
  alternation, zeroOrOne, zeroOrMore, oneOrMore
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
anyChar = StateB $ state AnyChar

anyString :: StateB
anyString = StateB $ loop ZeroOrMore anyChar

literalChar :: Char -> StateB
literalChar = StateB . state . LiteralChar

charRange :: (Char, Char) -> StateB
charRange = StateB . state . CharRange . sortPair

alternation :: StateB -> StateB -> StateB
alternation l r = StateB $ branch l r

zeroOrOne :: StateB -> StateB
zeroOrOne sb = StateB $ branch mempty sb

zeroOrMore :: StateB -> StateB
zeroOrMore = StateB . loop ZeroOrMore

oneOrMore :: StateB -> StateB
oneOrMore = StateB . loop OneOrMore

branch :: StateB -> StateB -> State -> State
branch left right join = split (buildState left join) (buildState right join)

loop :: LoopBehavior -> StateB -> State -> State
loop behavior body exit =
  case behavior of
    ZeroOrMore -> entry
    OneOrMore  -> body'
  where body' = buildState body entry
        entry = split body' exit

state :: Match -> State -> State
state accepts next = State{accepts, next}

split :: State -> State -> State
split left right = Split{left, right}

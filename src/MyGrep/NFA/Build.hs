{-# LANGUAGE DerivingVia #-}

module MyGrep.NFA.Build where

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
anyString = loop ZeroOrMore anyChar

literalChar :: Char -> StateB
literalChar = StateB . State . PositiveMatch . LiteralChar

charRange :: (Char, Char) -> StateB
charRange = StateB . State . PositiveMatch . CharRange . sortPair

branch :: StateB -> StateB -> StateB
branch left right = StateB \join -> Split (buildState left join) (buildState right join)

oneOf :: [StateB] -> StateB
oneOf = foldr1 branch

noneOf :: [CharMatch] -> StateB
noneOf = StateB . State . NegativeMatch

zeroOrOne :: StateB -> StateB
zeroOrOne = branch mempty

zeroOrMore :: StateB -> StateB
zeroOrMore = loop ZeroOrMore

oneOrMore :: StateB -> StateB
oneOrMore = loop OneOrMore

loop :: LoopBehavior -> StateB -> StateB
loop behavior body = StateB \exit ->
  let body' = buildState body entry
      entry = Split body' exit
  in case behavior of ZeroOrMore -> entry
                      OneOrMore  -> body'

exactly :: Int -> StateB -> StateB
exactly n = mconcat . replicate n

atLeast :: Int -> StateB -> StateB
atLeast n sb = exactly n sb <> loop ZeroOrMore sb

atMost :: Int -> StateB -> StateB
atMost n _  | n < 1 = mempty
atMost n sb         = zeroOrOne $ sb <> atMost (n - 1) sb

between :: Int -> Int -> StateB -> StateB
between min max sb = requireds <> optionals
  where requireds = exactly min sb
        optionals = atMost (max - min) sb

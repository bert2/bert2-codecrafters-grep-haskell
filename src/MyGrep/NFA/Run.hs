module MyGrep.NFA.Run (runNFA) where

import Control.Monad (liftM2)
import Data.HashSet qualified as Set (empty, insert, member)
import Data.HashSet (HashSet)
import Data.Function ((&))
import Data.Maybe (catMaybes)
import MyGrep.NFA.Base
import MyGrep.NFA.Eval

runNFA :: State -> String -> Bool
runNFA start = elem Final . foldl consume (expand start)

consume :: [State] -> Char -> [State]
consume ss char =
  ss & map (consume' char) & catMaybes & concatMap expand
  where consume' :: Char -> State -> Maybe State
        consume' c (State m n) | matches m c = Just n
        consume' _ _                         = Nothing

expand :: State -> [State]
expand = evalNFA visitedDefault evalUnvisited
  where visitedDefault :: State -> EvalState [State]
        visitedDefault _ = return []
        evalUnvisited :: ((State -> EvalState [State]) -> State -> EvalState [State])
        evalUnvisited evalNext = \case
          (Split l r) -> liftM2 (++) (evalNext l) (evalNext r)
          s           -> return [s]

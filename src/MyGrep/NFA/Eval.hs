module MyGrep.NFA.Eval (evalNFA, getOrMakeStateId, EvalState) where

import Control.Monad.State.Lazy qualified as M
import Data.HashMap.Lazy qualified as Map (empty, insert, lookup)
import Data.HashMap.Lazy (HashMap)
import Data.HashSet qualified as Set (empty, insert, member)
import Data.HashSet (HashSet)
import MyGrep.NFA.Base

type EvalState = M.State Context

data Context = Context { lastId :: Int, stateIds :: HashMap State Int, visited :: HashSet State}

evalNFA :: (State -> EvalState a)
        -> ((State -> EvalState a) -> State -> EvalState a)
        -> State
        -> a
evalNFA visitedDefault evalUnvisited s =
  M.evalState (evalNFA' visitedDefault evalUnvisited s) (Context 0 Map.empty Set.empty)
  where evalNFA' :: (State -> EvalState a)
                 -> ((State -> EvalState a) -> State -> EvalState a)
                 -> State
                 -> EvalState a
        evalNFA' visitedDefault evalUnvisited s = do
          visited <- wasVisited s
          if visited
            then visitedDefault s
            else do
              markVisited s
              evalUnvisited (evalNFA' visitedDefault evalUnvisited) s

markVisited :: State -> EvalState ()
markVisited s = do
  Context{..} <- M.get
  let visited' = Set.insert s visited
  M.put $ Context lastId stateIds visited'

wasVisited :: State -> EvalState Bool
wasVisited s = do
  Context{visited} <- M.get
  return $ Set.member s visited

getOrMakeStateId :: State -> EvalState Int
getOrMakeStateId s = getStateId s >>= maybe (makeStateId s) return

getStateId :: State -> EvalState (Maybe Int)
getStateId s = do
  Context{stateIds} <- M.get
  return $ Map.lookup s stateIds

makeStateId :: State -> EvalState Int
makeStateId s = do
  Context{..} <- M.get
  let lastId' = lastId + 1
  let stateIds' = Map.insert s lastId' stateIds
  M.put $ Context lastId' stateIds' visited
  return lastId'
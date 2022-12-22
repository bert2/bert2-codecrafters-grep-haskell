module MyGrep.NFA.Show where

import MyGrep.NFA.Base
import MyGrep.NFA.Eval

instance Show State where
  show = evalNFA visitedDefault evalUnvisited
    where visitedDefault :: State -> EvalState String
          visitedDefault s = ("#" ++) . show <$> getOrMakeStateId s
          evalUnvisited :: ((State -> EvalState String) -> State -> EvalState String)
          evalUnvisited evalNext s = do
            sid <- getOrMakeStateId s
            case s of
              Final -> return "Final"
              (State m n) -> do
                n' <- evalNext n
                return $ "State#" ++ show sid ++ " {accepts = " ++ show m ++ ", next = " ++ n' ++ "}"
              (Split l r) -> do
                l' <- evalNext l
                r' <- evalNext r
                return $ "Split#" ++ show sid ++ " {left = " ++ l' ++ ", right = " ++ r' ++ "}"

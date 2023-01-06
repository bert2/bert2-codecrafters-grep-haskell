module MyGrep.NFA.Print (printDotScript, printGravizoLink) where

import Data.List (intersperse)
import MyGrep.NFA.Base
import MyGrep.NFA.Eval
import MyGrep.URI qualified as URI

data Edge = Edge {from :: String, to :: String, lbl :: Maybe String}

printGravizoLink :: State -> String
printGravizoLink = ("https://g.gravizo.com/svg?" ++) . URI.encode . printDotScript

printDotScript :: State -> String
printDotScript nfa =
  let edges = mconcat . intersperse ";" . map printEdge $ collectEdges nfa
  in "digraph NFA{" ++ edges ++ "}"

printEdge :: Edge -> String
printEdge Edge{..} = from ++ "->" ++ to ++ maybe "" lblAttr lbl
  where lblAttr :: String -> String
        lblAttr lbl = "[label=\"" ++ lbl ++ "\"]"

collectEdges :: State -> [Edge]
collectEdges = evalNFA visitedDefault evalUnvisited
  where visitedDefault :: State -> EvalState [Edge]
        visitedDefault _ = return []
        evalUnvisited :: (State -> EvalState [Edge]) -> State -> EvalState [Edge]
        evalUnvisited evalNext s = case s of
          Final -> return []
          State{..} -> do
            edge <- makeEdge s next $ Just accepts
            edges <- evalNext next
            return $ edge:edges
          Split{..} -> do
            leftEdge <- makeEdge s left Nothing
            leftEdges <- evalNext left
            rightEdge <- makeEdge s right Nothing
            rightEdges <- evalNext right
            return $ leftEdge:leftEdges ++ rightEdge:rightEdges

makeEdge :: State -> State -> Maybe Match -> EvalState Edge
makeEdge from to accepts = do
  fromId <- show <$> getOrMakeStateId from
  toId <- case to of
    Final -> return "F"
    _     -> show <$> getOrMakeStateId to
  let lbl = printMatch <$> accepts
  return $ Edge fromId toId lbl

printMatch :: Match -> String
printMatch m = case m of
  AnyChar -> "*"
  (LiteralChar c) -> esc c
  (CharRange (min, max)) -> "[" ++ esc min ++ "-" ++ esc max ++ "]"
  where esc :: Char -> String
        esc '\\' = "\\\\"
        esc '"'  = "\\\""
        esc c    = [c]

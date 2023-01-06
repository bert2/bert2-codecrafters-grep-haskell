module Main where

import Control.Monad (when)
import Data.Either
import MyGrep.Args qualified as Args
import MyGrep.NFA.Base
import MyGrep.NFA.Build
import MyGrep.NFA.Eval
import MyGrep.NFA.Print
import MyGrep.NFA.Run
import MyGrep.NFA.Show
import MyGrep.Parser
import System.Exit

main :: IO ()
main = do
  let usage = "'./hs-grep-clone-exe -E pattern [input]'"
  Args.expectSwitchOrExit "-E" usage
  printGraph <- Args.hasSwitch "-g"
  pattern <- Args.getPositionalOrExit 0 usage
  input <- Args.getPositionalOrPrompt 1 "Input: "

  nfab <- either die return $ parseRegex pattern
  let nfa = buildNFA nfab
  -- putStrLn $ show nfa
  -- putStrLn $ printDotScript nfa

  when printGraph do
    putStrLn $ printGravizoLink nfa

  if runNFA nfa input
    then exitSuccess
    else exitFailure

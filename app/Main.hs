{-# LANGUAGE NamedFieldPuns #-}

module Main where

import System.Exit
import qualified Args

main :: IO ()
main = do
  let usage = "'./hs-grep-clone-exe -E pattern [input]'"
  Args.expectOrExit "-E" 0 usage
  pattern <- Args.getOrExit 1 usage
  input <- Args.getOrPrompt 2 "Input: "

  if matchPattern pattern input
    then exitSuccess
    else exitFailure

matchPattern :: String -> String -> Bool
matchPattern pattern input =
  if length pattern == 1
    then head pattern `elem` input
    else error $ "Unhandled pattern: " ++ pattern

module MyGrep.Args (
    expectSwitchOrExit,
    getPositionalOrExit,
    getPositionalOrPrompt,
    hasSwitch
  ) where

import System.Environment
import System.Exit
import System.IO

hasSwitch :: String -> IO Bool
hasSwitch sw = do
  (switches, _) <- parseArgs
  return $ sw `elem` switches

expectSwitchOrExit :: String -> String -> IO ()
expectSwitchOrExit sw usage = do
  has <- hasSwitch sw
  if has then return ()
  else printAndExit usage $ "expected switch '" ++ sw ++ "'"

getPositionalOrExit :: Int -> String -> IO String
getPositionalOrExit i usage = do
  (_, positionals) <- parseArgs
  if i >= length positionals
    then printAndExit usage $ "missing argument at position " ++ show i
    else return $ positionals !! i

getPositionalOrPrompt :: Int -> String -> IO String
getPositionalOrPrompt i prompt = do
  (_, positionals) <- parseArgs
  if i >= length positionals
    then printAndGetLine prompt
    else return $ positionals !! i

parseArgs :: IO ([String], [String])
parseArgs = do
  args <- getArgs
  return $ span isSwitch args
    where isSwitch :: String -> Bool
          isSwitch arg = length arg == 2 && head arg == '-'

printAndExit :: String -> String -> IO a
printAndExit usage error = do
  putStrLn $ "ERROR: " ++ error ++ "."
  putStrLn $ "Usage: " ++ usage
  exitFailure

printAndGetLine :: String -> IO String
printAndGetLine text = do
  putStr text
  hFlush stdout
  getLine

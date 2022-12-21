module Args (expectOrExit, getOrExit, getOrPrompt) where

import System.Environment
import System.Exit
import System.IO

expectOrExit :: String -> Int -> String -> IO ()
expectOrExit value i usage = do
  args <- getArgs
  if i >= length args
    then printAndExit usage $ "expected argument '" ++ value ++ "' at position " ++ (show i)
    else if args !! i /= value
      then printAndExit usage $ "expected argument '" ++ value ++ "' instead of '" ++ (args !! i) ++ "' at position " ++ (show i)
      else return ()

getOrExit :: Int -> String -> IO String
getOrExit i usage = do
  args <- getArgs
  if i >= length args
    then printAndExit usage $ "missing argument at position " ++ (show i)
    else return $ args !! i

getOrPrompt :: Int -> String -> IO String
getOrPrompt i prompt = do
  args <- getArgs
  if i >= length args
    then printAndGetLine prompt
    else return $ args !! i

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

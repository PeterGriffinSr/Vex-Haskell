module Main where

import Lexer
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      src <- readFile fileName
      case lexer fileName src src 1 1 of
        Left err -> putStrLn err
        Right tokens -> mapM_ print tokens
    _ -> putStrLn "Usage: Vex <filename>"

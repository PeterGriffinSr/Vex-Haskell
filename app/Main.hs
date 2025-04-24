module Main where

import System.Environment (getArgs)
import System.IO ()
import Lexer

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      src <- readFile fileName
      let tokens = lexer src
      mapM_ print tokens
    _ -> putStrLn "Usage: Vex <filename>"
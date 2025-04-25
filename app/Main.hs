module Main where

import AST (prettyExpr)
import Lexer
import Parser (handleParseError, parseExpr)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      src <- readFile fileName
      case lexer fileName src src 1 1 of
        Left err -> putStrLn err >> exitFailure
        Right tokens -> do
          putStrLn "Tokens:"
          mapM_ print tokens
          putStrLn "\nParsed expression:"
          case parseExpr fileName src of
            Left errBundle -> handleParseError fileName src errBundle >> exitFailure
            Right ast -> putStrLn (prettyExpr ast)
    _ -> putStrLn "Usage: Vex <filename>"

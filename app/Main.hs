module Main where

import Lexer

main :: IO ()
main = do
    let code = "let x = 3.14\nin \"hi\" // comment\n"
    print $ lexer code
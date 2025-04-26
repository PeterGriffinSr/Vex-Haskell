module Error where

import Color (blue, red, reset)
import System.Exit (exitFailure)

prettyError :: String -> String -> Int -> Int -> String -> String
prettyError file src l c msg =
  let ls = lines src
      lineStr = if l <= length ls then ls !! (l - 1) else ""
      caret = replicate (c - 1) ' ' ++ "^"
   in unlines
        [ red ++ "error: " ++ reset ++ msg,
          blue ++ "    --> " ++ reset ++ file ++ ":" ++ show l ++ ":" ++ show c,
          blue ++ "    |" ++ reset,
          blue ++ "    |  " ++ reset ++ lineStr,
          blue ++ "    |  " ++ red ++ caret ++ reset,
          blue ++ "    |" ++ reset
        ]

unrecognizedFlag :: String -> IO ()
unrecognizedFlag flag = do
  putStrLn $
    "Vex: " ++ red ++ "error: " ++ reset ++ "unrecognized command-line option '" ++ flag ++ "'" ++ reset
  exitFailure

noInputFile :: IO ()
noInputFile = do
  putStrLn $ "Vex: " ++ red ++ "fatal error: " ++ reset ++ "no input files" ++ reset
  exitFailure

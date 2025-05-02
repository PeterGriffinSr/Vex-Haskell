module Vex.Core.Error where

import System.Exit (exitFailure)
import Vex.CLI.Color (blue, red, reset)

prettyError :: String -> String -> Int -> Int -> String -> [String] -> String
prettyError file sourceCode lineNumber columnNumber message notes =
  let sourceLines = lines sourceCode
      errorLine =
        if lineNumber > 0 && lineNumber <= length sourceLines
          then sourceLines !! (lineNumber - 1)
          else "<line out of bounds>"
      caretIndicator = replicate (max 0 (columnNumber - 1)) ' ' ++ "^"
      formattedNotes = map (\n -> blue ++ "note:" ++ reset ++ " " ++ n) notes
   in unlines $
        [ red ++ "error:" ++ reset ++ " " ++ message,
          blue ++ "   --> " ++ reset ++ file ++ ":" ++ show lineNumber ++ ":" ++ show columnNumber,
          blue ++ "    |" ++ reset,
          blue ++ padLeft 4 (show lineNumber) ++ "|  " ++ reset ++ errorLine,
          blue ++ "    |  " ++ replicate (columnNumber - 1) ' ' ++ red ++ "^" ++ reset
        ]
          ++ formattedNotes

padLeft :: Int -> String -> String
padLeft n s = replicate (n - length s) ' ' ++ s

unrecognizedFlag :: String -> IO ()
unrecognizedFlag flag = do
  putStrLn $
    "Vex: " ++ red ++ "error: " ++ reset ++ "unrecognized command-line option '" ++ flag ++ "'" ++ reset
  exitFailure

noInputFile :: IO ()
noInputFile = do
  putStrLn $ "Vex: " ++ red ++ "fatal error: " ++ reset ++ "no input files" ++ reset
  exitFailure

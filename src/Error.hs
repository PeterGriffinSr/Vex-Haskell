module Error where

import Color

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

module Error where

import Color

prettyError :: String -> String -> Int -> Int -> String -> String
prettyError filePath sourceCode line col errMsg =
  let codeLines = lines sourceCode
      badLine = if line <= length codeLines then codeLines !! (line - 1) else ""
      caretLine = replicate (col - 1) ' ' ++ "^"
   in unlines
        [ red ++ "error: " ++ reset ++ errMsg,
          blue ++ "    --> " ++ reset ++ filePath ++ ":" ++ show line ++ ":" ++ show col,
          blue ++ "    |" ++ reset, 
          blue ++ "    |  " ++ reset ++ badLine,
          blue ++ "    |  " ++ red ++ caretLine ++ reset,
          blue ++ "    |" ++ reset
        ]

module Error where

prettyError :: String -> String -> Int -> Int -> String -> String
prettyError filePath sourceCode line col errMsg =
  let codeLines = lines sourceCode
      badLine = if line <= length codeLines then codeLines !! (line - 1) else ""
      caretLine = replicate (col - 1) ' ' ++ "^"
   in unlines
        [ "error: " ++ errMsg,
          "    --> " ++ filePath ++ ":" ++ show line ++ ":" ++ show col,
          "    |",
          "    |  " ++ badLine,
          "    |  " ++ caretLine,
          "    |"
        ]

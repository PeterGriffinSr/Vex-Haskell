module Vex.Compiler.Lexer (lexer) where

import Data.Char (isAlpha, isDigit, isSpace)
import Data.Maybe (listToMaybe)
import Vex.Compiler.Token (Token (..))
import Vex.Core.Error (prettyError)

lexer :: String -> String -> String -> Int -> Int -> Either String [Token]
lexer file src [] _ _ = Right [TokEOF]
lexer file src (c : cs) l col
  | isSpace c = lexer file src cs (if c == '\n' then l + 1 else l) (if c == '\n' then 1 else col + 1)
  | isDigit c = lexNumber file src (c : cs) l col
  | isAlpha c = lexIdent file src (c : cs) l col
  | c == '"' = lexString file src cs l col
  | c == '\'' = lexChar file src cs l col
  | c == '#' = lexComment file src cs l
  | otherwise = case (c, cs) of
      ('-', '>' : r) -> tok TokArrow r 2
      ('-', '.' : r) -> tok TokFMinus r 2
      ('-', r) -> tok TokMinus r 1
      ('+', '.' : r) -> tok TokFPlus r 2
      ('+', r) -> tok TokPlus r 1
      ('*', '.' : r) -> tok TokFMul r 2
      ('*', r) -> tok TokMul r 1
      ('/', '.' : r) -> tok TokFDiv r 2
      ('/', r) -> tok TokDiv r 1
      ('.', '.' : '.' : r) -> tok TokSpread r 3
      ('.', r) -> tok TokDot r 1
      ('=', '>' : r) -> tok TokRArrow r 2
      ('=', '=' : r) -> tok TokEq r 2
      ('=', r) -> tok TokAssign r 1
      ('!', '=' : r) -> tok TokNeq r 2
      ('!', r) -> tok TokNot r 1
      ('<', '=' : r) -> tok TokLeq r 2
      ('<', r) -> tok TokLess r 1
      ('>', '=' : r) -> tok TokGeq r 2
      ('>', r) -> tok TokGreater r 1
      ('(', r) -> tok TokLParen r 1
      (')', r) -> tok TokRParen r 1
      ('[', r) -> tok TokLBracket r 1
      (']', r) -> tok TokRBracket r 1
      ('{', r) -> tok TokLBrace r 1
      ('}', r) -> tok TokRBrace r 1
      (':', r) -> tok TokColon r 1
      (';', r) -> tok TokSemi r 1
      (',', r) -> tok TokComma r 1
      ('_', r) -> tok TokUnderScore r 1
      _ ->
        Left $
          prettyError
            file
            src
            l
            col
            "Unexpected character"
            [ "This character does not belong to the lexer's terminal alphabet Σ",
              "Lexing requires all characters to be members of a known lexical class",
              "Ensure the source string ∈ L, the language defined by the Vex grammar"
            ]
  where
    tok t rest n = (t :) <$> lexer file src rest l (col + n)

lexNumber :: String -> String -> String -> Int -> Int -> Either String [Token]
lexNumber file src cs l col =
  let (n, rest) = span (\c -> isDigit c || c == '.') cs
      dots = length $ filter (== '.') n
   in case dots of
        0 -> (TokIntLit (read n) :) <$> lexer file src rest l (col + length n)
        1 -> (TokFloatLit (read n) :) <$> lexer file src rest l (col + length n)
        _ ->
          Left $
            prettyError
              file
              src
              l
              col
              "Invalid number with multiple dots"
              [ "A numeric literal must conform to: Int | Int.Float",
                "Multiple decimal points introduce ambiguity in the token stream",
                "Lexer invariant: ∃! '.' in any floating-point token"
              ]

lexIdent :: String -> String -> String -> Int -> Int -> Either String [Token]
lexIdent file src cs l col =
  let (i, rest) = span isAlpha cs
      kw = case i of
        "val" -> TokVal
        "fn" -> TokFn
        "match" -> TokMatch
        "with" -> TokWith
        "if" -> TokIf
        "then" -> TokThen
        "else" -> TokElse
        "None" -> TokNone
        "Some" -> TokSome
        "Ok" -> TokOk
        "Error" -> TokError
        "int" -> TokInt
        "float" -> TokFloat
        "char" -> TokChar
        "bool" -> TokBool
        "string" -> TokString
        "true" -> TokBoolLit True
        "false" -> TokBoolLit False
        "print" -> TokPrint
        _ -> TokIdent i
   in (kw :) <$> lexer file src rest l (col + length i)

lexString :: String -> String -> String -> Int -> Int -> Either String [Token]
lexString file src cs l col =
  let (s, rest) = span (/= '"') cs
   in case rest of
        ('"' : r) ->
          if null s
            then
              Left $
                prettyError
                  file
                  src
                  l
                  col
                  "Empty string literal"
                  [ "While allowed (\"\" ∈ String), empty strings may be unintended",
                    "Consider: did you mean to write a nonempty string?"
                  ]
            else
              let (processedStr, errors) = processEscapes s
               in if null errors
                    then (TokStringLit processedStr :) <$> lexer file src r l (col + length s + 2)
                    else case listToMaybe errors of
                      Just err ->
                        Left $
                          prettyError
                            file
                            src
                            l
                            col
                            ("Invalid escape sequence: " ++ err)
                            [ "The lexer accepts Σ_escape = {'\\n', '\\t', '\\r', '\\\"', '\\\\'}",
                              "Escape sequences must match elements of Σ_escape"
                            ]
                      Nothing ->
                        Left $
                          prettyError
                            file
                            src
                            l
                            col
                            "Unknown string escape error"
                            [ "Escape processing failed unexpectedly",
                              "Please check the string syntax and escape structure"
                            ]
        _ ->
          Left $
            prettyError
              file
              src
              l
              col
              "Unterminated string"
              [ "Reached end of input without encountering a closing quote (\")",
                "A string literal must conform to: '\"' Σ* '\"'"
              ]

processEscapes :: String -> (String, [String])
processEscapes [] = ([], [])
processEscapes ('\\' : c : cs) =
  let (rest, errs) = processEscapes cs
   in case c of
        'n' -> ('\n' : rest, errs)
        't' -> ('\t' : rest, errs)
        'r' -> ('\r' : rest, errs)
        '"' -> ('\"' : rest, errs)
        '\\' -> ('\\' : rest, errs)
        _ -> (rest, ["\\" ++ [c] ++ " ∉ Σ_escape"])
processEscapes (c : cs) =
  let (rest, errs) = processEscapes cs
   in (c : rest, errs)

lexChar :: String -> String -> String -> Int -> Int -> Either String [Token]
lexChar file src cs l col =
  let (ch, rest) = span (/= '\'') cs
   in case rest of
        ('\'' : r) -> case listToMaybe ch of
          Just c -> (TokCharLit c :) <$> lexer file src r l (col + length ch + 2)
          Nothing ->
            Left $
              prettyError
                file
                src
                l
                col
                "Empty character literal"
                [ "A character literal must encode exactly one Unicode scalar value",
                  "Form: '\'' c '\'' for some c ∈ Char"
                ]
        _ ->
          Left $
            prettyError
              file
              src
              l
              col
              "Unterminated character"
              [ "Lexical invariant violated: expected closing quote (')",
                "A character literal must match the regular expression: '\'' . '\''"
              ]

lexComment :: String -> String -> String -> Int -> Either String [Token]
lexComment file src cs l = lexer file src (dropWhile (/= '\n') cs) (l + 1) 1

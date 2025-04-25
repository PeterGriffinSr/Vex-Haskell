module Lexer (lexer) where

import Data.Char
import Error (prettyError)
import Token

lexer :: String -> String -> String -> Int -> Int -> Either String [Token]
lexer filename fullSrc [] _ _ = Right [TokEOF]
lexer filename fullSrc (c : cs) line col
  | isSpace c = case c of
      '\n' -> lexer filename fullSrc cs (line + 1) 1
      _ -> lexer filename fullSrc cs line (col + 1)
  | isDigit c = lexNumber filename fullSrc (c : cs) line col
  | isAlpha c = lexIdent filename fullSrc (c : cs) line col
  | c == '"' = lexString filename fullSrc cs line col
  | c == '\'' = lexChar filename fullSrc cs line col
  | c == '#' = lexComment filename fullSrc cs line
  | c == '-' = case cs of
      ('>' : rest) -> addToken TokArrow rest 2
      ('.' : rest) -> addToken ToKFMinus rest 2
      _ -> addToken TokMinus cs 1
  | c == '+' = case cs of
      ('.' : rest) -> addToken TokFPlus rest 2
      _ -> addToken TokPlus cs 1
  | c == '*' = case cs of
      ('.' : rest) -> addToken TokFMul rest 2
      _ -> addToken TokMul cs 1
  | c == '/' = case cs of
      ('.' : rest) -> addToken TokFDiv rest 2
      _ -> addToken TokDiv cs 1
  | c == '(' = addToken TokLParen cs 1
  | c == ')' = addToken TokRParen cs 1
  | c == '[' = addToken TokLBracket cs 1
  | c == ']' = addToken TokRBracket cs 1
  | c == '{' = addToken TokLBrace cs 1
  | c == '}' = addToken TokRBrace cs 1
  | c == '_' = addToken TokUnderScore cs 1
  | c == '.' = case cs of
      ('.' : '.' : rest) -> addToken TokSpread rest 3
      _ -> addToken TokDot cs 1
  | c == ':' = addToken TokColon cs 1
  | c == ';' = addToken TokSemi cs 1
  | c == '=' = case cs of
      ('>' : rest) -> addToken TokRArrow rest 2
      ('=' : rest) -> addToken TokEq rest 2
      _ -> addToken TokAssign cs 1
  | c == '!' = case cs of
      ('=' : rest) -> addToken TokNeq rest 2
      _ -> addToken TokNot cs 1
  | c == '<' = case cs of
      ('=' : rest) -> addToken TokLeq rest 2
      _ -> addToken TokLess cs 1
  | c == '>' = case cs of
      ('=' : rest) -> addToken TokGeq rest 2
      _ -> addToken TokGreater cs 1
  | c == ',' = addToken TokComma cs 1
  | otherwise = Left $ prettyError filename fullSrc line col "Unexpected character"
  where
    addToken tok rest offset = do
      more <- lexer filename fullSrc rest line (col + offset)
      return (tok : more)

lexNumber :: String -> String -> String -> Int -> Int -> Either String [Token]
lexNumber filename fullSrc cs line col =
  let (numPart, rest) = span (\c -> isDigit c || c == '.') cs
      dotCount = length (filter (== '.') numPart)
   in case dotCount of
        0 -> do
          more <- lexer filename fullSrc rest line (col + length numPart)
          return (TokIntLit (read numPart) : more)
        1 -> do
          more <- lexer filename fullSrc rest line (col + length numPart)
          return (TokFloatLit (read numPart) : more)
        _ -> Left $ prettyError filename fullSrc line col "Invalid number with multiple dots"

lexIdent :: String -> String -> String -> Int -> Int -> Either String [Token]
lexIdent filename fullSrc cs line col =
  let (ident, rest) = span isAlpha cs
      tok = case ident of
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
        _ -> TokIdent ident
   in do
        more <- lexer filename fullSrc rest line (col + length ident)
        return (tok : more)

lexString :: String -> String -> String -> Int -> Int -> Either String [Token]
lexString filename fullSrc cs line col =
  let (str, rest) = span (/= '"') cs
   in case rest of
        ('"' : rest') -> do
          more <- lexer filename fullSrc rest' line (col + length str + 2)
          return (TokStringLit str : more)
        _ -> Left $ prettyError filename fullSrc line col "Unterminated string"

lexChar :: String -> String -> String -> Int -> Int -> Either String [Token]
lexChar filename fullSrc cs line col =
  let (char, rest) = span (/= '\'') cs
   in case rest of
        ('\'' : rest') -> do
          more <- lexer filename fullSrc rest' line (col + length char + 2)
          return (TokCharLit (head char) : more)
        _ -> Left $ prettyError filename fullSrc line col "Unterminated character"

lexComment :: String -> String -> String -> Int -> Either String [Token]
lexComment filename fullSrc cs line = lexer filename fullSrc (dropWhile (/= '\n') cs) (line + 1) 1

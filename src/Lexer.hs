-- Lexer.hs
module Lexer (lexer) where

import Token
import Data.Char

lexer :: String -> [Token]
lexer [] = [TokEOF]
lexer (c:cs)
    | isSpace c       = lexer cs
    | isDigit c       = lexNumber (c:cs)
    | isAlpha c       = lexIdent (c:cs)
    | c == '"'        = lexString cs
    | c == '\''       = lexChar cs
    | c == '#'        = lexComment cs
    | c == '-'        = TokMinus : lexer cs
    | c == '+'        = TokPlus  : lexer cs
    | c == '*'        = TokMul   : lexer cs
    | c == '/'        = TokDiv   : lexer cs
    | c == '('        = TokLParen : lexer cs
    | c == ')'        = TokRParen : lexer cs
    | c == ':'        = TokColon : lexer cs
    | c == ';'        = TokSemi : lexer cs
    | c == '='        = TokAssign : lexer cs
    | otherwise       = error ("Unrecognized character: " ++ [c])

lexNumber :: [Char] -> [Token]
lexNumber cs =
    let (numPart, rest) = span (\c -> isDigit c || c == '.') cs
        dotCount = length (filter (=='.') numPart)
    in case dotCount of
        0 -> TokIntLit (read numPart) : lexer rest
        1 -> TokFloatLit (read numPart) : lexer rest
        _ -> error ("Invalid number with multiple dots: " ++ numPart)

lexIdent :: [Char] -> [Token]
lexIdent cs =
    let (ident, rest) = span isAlpha cs
    in case ident of
        "val"    -> TokVal : lexer rest
        "int"    -> TokInt : lexer rest
        "float"  -> TokFloat : lexer rest
        "char"   -> TokChar : lexer rest
        "bool"   -> TokBool : lexer rest
        "string" -> TokString : lexer rest
        "true"   -> TokBoolLit True : lexer rest
        "false"  -> TokBoolLit False : lexer rest
        _        -> TokIdent ident : lexer rest

lexString :: [Char] -> [Token]
lexString cs = 
    let (str, rest) = span (/= '"') cs
    in case rest of
        ('"':rest') -> TokStringLit str : lexer rest'
        _ -> error "Unterminated string literal"

lexChar :: [Char] -> [Token]
lexChar cs =
    let (char, rest) = span (/= '\'') cs
    in case rest of
        ('\'':rest') -> TokCharLit (head char) : lexer rest'
        _ -> error "Unterminated char literal"


lexComment :: [Char] -> [Token]
lexComment cs =
    lexer (dropWhile (/= '\n') cs)

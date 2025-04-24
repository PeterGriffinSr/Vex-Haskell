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
    | c == '-'        = TokMinus : lexer cs
    | c == '+'        = TokPlus  : lexer cs
    | c == '*'        = TokMul   : lexer cs
    | c == '/'        = lexCommentOrDiv cs
    | c == '('        = TokLParen : lexer cs
    | c == ')'        = TokRParen : lexer cs
    | c == '='        = TokEq : lexer cs
    | otherwise       = error ("Unrecognized character: " ++ [c])

lexNumber :: [Char] -> [Token]
lexNumber cs =
    let (numPart, rest) = span (\c -> isDigit c || c == '.') cs
        dotCount = length (filter (=='.') numPart)
    in case dotCount of
        0 -> TokInt (read numPart) : lexer rest
        1 -> TokFloat (read numPart) : lexer rest
        _ -> error ("Invalid number with multiple dots: " ++ numPart)

lexIdent :: [Char] -> [Token]
lexIdent cs =
    let (ident, rest) = span isAlpha cs
    in case ident of
        "let" -> TokLet : lexer rest
        "in"  -> TokIn  : lexer rest
        _     -> TokIdent ident : lexer rest

lexString :: [Char] -> [Token]
lexString cs = 
    let (str, rest) = span (/= '"') cs
    in case rest of
        ('"':rest') -> TokString str : lexer rest'
        _ -> error "Unterminated string literal"

lexCommentOrDiv :: [Char] -> [Token]
lexCommentOrDiv ('/':'/':rest) =
    lexer (dropWhile (/= '\n') rest)
lexCommentOrDiv cs =
    TokDiv : lexer cs

module Token where

data Token = 
    TokInt Int
    | TokIdent String
    | TokString String
    | TokFloat Double
    | TokPlus | TokMinus | TokMul | TokDiv
    | TokLParen | TokRParen
    | TokAssign | TokLet | TokIn | TokEq
    | TokEOF
    deriving (Show, Eq)

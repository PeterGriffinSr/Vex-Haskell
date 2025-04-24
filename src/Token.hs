module Token where

data Token = 
    TokIntLit Int
    | TokIdent String
    | TokStringLit String
    | TokFloatLit Double
    | TokCharLit Char
    | TokBoolLit Bool
    | TokPlus | TokMinus | TokMul | TokDiv | TokSemi
    | TokLParen | TokRParen | TokColon
    | TokAssign | TokVal | TokInt | TokString | TokChar | TokFloat | TokBool
    | TokEOF
    deriving (Show, Eq)

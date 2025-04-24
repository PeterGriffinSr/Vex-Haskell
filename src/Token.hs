module Token where

data Token
  = TokIntLit Int
  | TokIdent String
  | TokStringLit String
  | TokFloatLit Double
  | TokCharLit Char
  | TokBoolLit Bool
  | ---
    TokPlus
  | TokMinus
  | TokMul
  | TokDiv
  | TokSemi
  | TokFPlus
  | ToKFMinus
  | TokFMul
  | TokFDiv
  | TokLParen
  | TokRParen
  | TokColon
  | TokAssign
  | TokArrow
  | TokRArrow
  | TokEq
  | TokNeq
  | TokGeq
  | TokLeq
  | TokLess
  | TokGreater
  | TokNot
  | TokComma
  | TokLBracket
  | TokRBracket
  | TokLBrace
  | TokRBrace
  | TokUnderScore
  | TokDot
  | TokSpread
  | ---
    TokVal
  | TokFn
  | TokInt
  | TokString
  | TokChar
  | TokFloat
  | TokBool
  | TokPrint
  | TokMatch
  | TokWith
  | TokIf
  | TokThen
  | TokElse
  | TokNone
  | TokSome
  | TokOk
  | TokError
  | ---
    TokEOF
  deriving (Show, Eq)

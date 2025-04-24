module Token where

data Token
  = TokIntLit Int
  | TokIdent String
  | TokStringLit String
  | TokFloatLit Double
  | TokCharLit Char
  | TokBoolLit Bool
  | TokPlus
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
  | TokVal
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
  | TokInt
  | TokString
  | TokChar
  | TokFloat
  | TokBool
  | TokPrint
  | TokFn
  | TokEOF
  deriving (Show, Eq)

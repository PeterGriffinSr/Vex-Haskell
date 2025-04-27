module Vex.Core.AST (prettyExpr, TypeName (..), Expr (..)) where

type Name = String

data TypeName
  = TokInt
  | TokFloat
  | TokString
  | TokChar
  | TokBool
  deriving (Show, Eq)

data Expr
  = IntLit Integer
  | FloatLit Double
  | BoolLit Bool
  | CharLit Char
  | StringLit String
  | Var Name
  | UnaryOp String Expr
  | BinaryOp String Expr Expr
  | Call Expr [Expr]
  | Parens Expr
  | VarDecl (Maybe TypeName) String Expr
  deriving (Show, Eq)

prettyExpr :: Expr -> String
prettyExpr = prettyExprWithIndent 0

prettyExprWithIndent :: Int -> Expr -> String
prettyExprWithIndent indent expr =
  let ind = replicate indent ' '
      next = prettyExprWithIndent (indent + 2)
   in case expr of
        IntLit n -> ind ++ "IntLit " ++ show n
        FloatLit f -> ind ++ "FloatLit " ++ show f
        BoolLit b -> ind ++ "BoolLit " ++ show b
        CharLit c -> ind ++ "CharLit '" ++ [c] ++ "'"
        StringLit s -> ind ++ "StringLit \"" ++ s ++ "\""
        Var name -> ind ++ "Var " ++ name
        UnaryOp op e -> ind ++ "UnaryOp " ++ op ++ "\n" ++ next e
        BinaryOp op l r -> ind ++ "BinaryOp " ++ op ++ "\n" ++ next l ++ "\n" ++ next r
        Call fn args -> ind ++ "Call\n" ++ next fn ++ concatMap (\a -> "\n" ++ next a) args
        Parens e -> ind ++ "Parens\n" ++ next e
        VarDecl mty name rhs -> ind ++ "VarDecl " ++ name ++ maybe "" (\ty -> " : " ++ show ty) mty ++ "\n" ++ next rhs

module Vex.Compiler.Parser (parseExpr, handleParseError) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (eof, notFollowedBy, try),
    ParseErrorBundle (bundleErrors, bundlePosState),
    Parsec,
    SourcePos (sourceColumn, sourceLine),
    anySingle,
    attachSourcePos,
    between,
    choice,
    empty,
    errorOffset,
    many,
    manyTill,
    parse,
    parseErrorTextPretty,
    sepEndBy1,
    unPos,
    (<|>),
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    letterChar,
    space1,
  )
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Pos (sourceColumn, sourceLine)
import Vex.Compiler.AST
  ( Expr
      ( BinaryOp,
        BoolLit,
        CharLit,
        FloatLit,
        IntLit,
        Parens,
        StringLit,
        UnaryOp,
        Var,
        VarDecl
      ),
    TypeName (..),
  )
import Vex.Core.Error (prettyError)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pInt :: Parser Expr
pInt = IntLit <$> lexeme L.decimal

pFloat :: Parser Expr
pFloat = try $ FloatLit <$> lexeme L.float

pBool :: Parser Expr
pBool = (BoolLit True <$ symbol "true") <|> (BoolLit False <$ symbol "false")

pChar :: Parser Expr
pChar = do
  _ <- char '\''
  c <- anySingle
  _ <- char '\''
  return $ CharLit c

pString :: Parser Expr
pString = StringLit <$> (char '"' *> manyTill L.charLiteral (char '"'))

pVar :: Parser Expr
pVar = Var <$> lexeme ((:) <$> letterChar <*> many alphaNumChar)

pTerm :: Parser Expr
pTerm = choice [try pFloat, pInt, pBool, pChar, pString, pVar, Parens <$> parens pExpr]

pValDecl :: Parser Expr
pValDecl = do
  _ <- symbol "val"
  typeAndName <- try typedWithColon
  _ <- symbol "="
  rhs <- pExpr
  case typeAndName of
    (ty, name) -> return $ VarDecl ty name rhs
  where
    typedWithColon = do
      ty <- typeName
      _ <- symbol ":"
      name <- lexeme identifier
      return (Just ty, name)

parseKeyword :: String -> Parser String
parseKeyword kw = symbol kw <* notFollowedBy alphaNumChar

identifier :: Parser String
identifier = (:) <$> letterChar <*> many alphaNumChar

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ InfixL (BinaryOp "*." <$ symbol "*."),
      InfixL (BinaryOp "/." <$ symbol "/."),
      InfixL (BinaryOp "*" <$ symbol "*"),
      InfixL (BinaryOp "/" <$ symbol "/")
    ],
    [ InfixL (BinaryOp "+." <$ symbol "+."),
      InfixL (BinaryOp "-." <$ symbol "-."),
      InfixL (BinaryOp "+" <$ symbol "+"),
      InfixL (BinaryOp "-" <$ symbol "-")
    ],
    [Prefix (UnaryOp "-" <$ symbol "-")]
  ]

typeName :: Parser TypeName
typeName =
  choice
    [ TokInt <$ parseKeyword "int",
      TokFloat <$ parseKeyword "float",
      TokString <$ parseKeyword "string",
      TokChar <$ parseKeyword "char",
      TokBool <$ parseKeyword "bool"
    ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

parseExpr :: String -> String -> Either (ParseErrorBundle String Void) [Expr]
parseExpr = parse (sc *> sepEndBy1 (pValDecl <|> pExpr) sc <* eof)

handleParseError :: String -> String -> ParseErrorBundle String Void -> IO ()
handleParseError filePath src err = do
  let posState = bundlePosState err
      errs = bundleErrors err
      (errsWithPos, _) = attachSourcePos errorOffset errs posState
      ((parseError, srcPos) :| _) = errsWithPos
      line = unPos (sourceLine srcPos)
      col = unPos (sourceColumn srcPos)
      msg = parseErrorTextPretty parseError
  putStrLn (prettyError filePath src line col msg)

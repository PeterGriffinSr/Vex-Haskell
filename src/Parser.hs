module Parser (parseExpr, handleParseError) where

import AST
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Void
import Error (prettyError)
import Text.Megaparsec
import Text.Megaparsec (ParseErrorBundle)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Pos (sourceColumn, sourceLine)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

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

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [Prefix (UnaryOp "-" <$ symbol "-")],
    [InfixL (BinaryOp "*" <$ symbol "*"), InfixL (BinaryOp "/" <$ symbol "/")],
    [InfixL (BinaryOp "+" <$ symbol "+"), InfixL (BinaryOp "-" <$ symbol "-")]
  ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

parseExpr :: String -> String -> Either (ParseErrorBundle String Void) Expr
parseExpr filePath src = parse (sc *> pExpr <* eof) filePath src

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

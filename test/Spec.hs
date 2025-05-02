import Control.Exception (evaluate)
import Test.Hspec
  ( anyErrorCall,
    describe,
    hspec,
    it,
    shouldBe,
    shouldThrow,
  )
import Vex.Compiler.Lexer (lexer)
import Vex.Compiler.Token (Token (..))
import qualified Vex.Compiler.Token as Token

main :: IO ()
main = hspec $ do
  describe "Lexer" $ do
    it "lexes a let binding with int" $
      lexerWrapper "val int: x = 42;"
        `shouldBe` [TokVal, Token.TokInt, TokColon, TokIdent "x", TokAssign, TokIntLit 42, TokSemi]

    it "lexes a float and operator" $
      lexerWrapper "3.14 + 2.0"
        `shouldBe` [TokFloatLit 3.14, TokPlus, TokFloatLit 2.0]

    it "lexes a string literal" $
      lexerWrapper "\"hello\""
        `shouldBe` [TokStringLit "hello"]

    it "lexes a char literal" $
      lexerWrapper "'H'"
        `shouldBe` [TokCharLit 'H']

    it "ignores single-line comments" $
      lexerWrapper "42 # comment here"
        `shouldBe` [TokIntLit 42]

    it "errors on unterminated string" $
      evaluate (lexerWrapper "\"oops") `shouldThrow` anyErrorCall

    it "errors on unterminated char" $
      evaluate (lexerWrapper "'oops") `shouldThrow` anyErrorCall

    it "errors on empty string literal" $
      evaluate (lexerWrapper "\"\"") `shouldThrow` anyErrorCall

    it "errors on empty character literal" $
      evaluate (lexerWrapper "''") `shouldThrow` anyErrorCall

    it "errors on invalid number with multiple dots" $
      evaluate (lexerWrapper "3.14.15") `shouldThrow` anyErrorCall

    it "errors on invalid escape sequence in string" $
      evaluate (lexerWrapper "\"hello\\xworld\"") `shouldThrow` anyErrorCall

    it "errors on unexpected character" $
      evaluate (lexerWrapper "@") `shouldThrow` anyErrorCall

lexerWrapper :: String -> [Token]
lexerWrapper src =
  case lexer "test.vex" src src 1 1 of
    Left err -> error err
    Right toks -> init toks

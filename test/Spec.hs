import Test.Hspec
import Lexer
import Control.Exception (evaluate)
import Token

main :: IO ()
main = hspec $ do
  describe "lexer" $ do
    it "lexes a let binding with int" $ do
      lexer "let x = 42" `shouldBe`
        [TokLet, TokIdent "x", TokEq, TokInt 42, TokEOF]

    it "lexes a float and operator" $ do
      lexer "3.14 + 2.0" `shouldBe`
        [TokFloat 3.14, TokPlus, TokFloat 2.0, TokEOF]

    it "lexes a string literal" $ do
      lexer "\"hello\"" `shouldBe`
        [TokString "hello", TokEOF]

    it "ignores single-line comments" $ do
      lexer "42 # comment here" `shouldBe`
        [TokInt 42, TokEOF]

    it "errors on unterminated string" $ do
      evaluate (lexer "\"oops") `shouldThrow` anyErrorCall
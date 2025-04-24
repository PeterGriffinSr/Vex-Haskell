import Test.Hspec
import Lexer
import Control.Exception (evaluate)
import Token

main :: IO ()
main = hspec $ do
  describe "lexer" $ do
    it "lexes a let binding with int" $ do
      lexer "val int: x = 42;" `shouldBe`
        [TokVal, TokInt, TokColon, TokIdent "x", TokAssign, TokIntLit 42, TokSemi, TokEOF]

    it "lexes a float and operator" $ do
      lexer "3.14 + 2.0" `shouldBe`
        [TokFloatLit 3.14, TokPlus, TokFloatLit 2.0, TokEOF]

    it "lexes a string literal" $ do
      lexer "\"hello\"" `shouldBe`
        [TokStringLit "hello", TokEOF]

    it "lexes a char literal" $ do
      lexer "'H'" `shouldBe`
        [TokCharLit 'H', TokEOF]

    it "ignores single-line comments" $ do
      lexer "42 # comment here" `shouldBe`
        [TokIntLit 42, TokEOF]

    it "errors on unterminated string" $ do
      evaluate (lexer "\"oops") `shouldThrow` anyErrorCall

    it "errors on unterminated char" $ do
      evaluate (lexer "\'oops") `shouldThrow` anyErrorCall
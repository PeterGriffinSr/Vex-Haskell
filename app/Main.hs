import AST (prettyExpr)
import Help (displayCompilerHelp, displayGeneral, displayOptimizersHelp, displayTargetHelp, displayVersion, displayWarningsHelp)
import Lexer
import Parser (handleParseError, parseExpr)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> displayGeneral
    ["--help=optimizers"] -> displayOptimizersHelp
    ["--help=warnings"] -> displayWarningsHelp
    ["--help=target"] -> displayTargetHelp
    ["--help=compiler"] -> displayCompilerHelp
    ["--version"] -> displayVersion
    ["repl"] -> putStrLn "Launching REPL..."
    [fileName] -> do
      src <- readFile fileName
      case lexer fileName src src 1 1 of
        Left err -> putStrLn err >> exitFailure
        Right tokens -> do
          putStrLn "Tokens:"
          mapM_ print tokens
          putStrLn "\nParsed expressions:"
          case parseExpr fileName src of
            Left errBundle -> handleParseError fileName src errBundle >> exitFailure
            Right asts -> mapM_ (putStrLn . prettyExpr) asts
    _ -> putStrLn "Usage: Vex <filename>"

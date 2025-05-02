module Vex.CLI.Cli where

import Vex.CLI.Help (displayCompilerHelp, displayGeneral, displayOptimizersHelp, displayTargetHelp, displayVersion, displayWarningsHelp)
import Vex.CLI.Options (Command (..), CompileMode (..), HelpTopic (..))
import Vex.CLI.Parser (parseArgs)
import Vex.Compiler.Compiler (compileFile)

handleArgs :: IO ()
handleArgs = do
  cmd <- parseArgs
  case cmd of
    Help Nothing -> displayGeneral
    Help (Just Optimizers) -> displayOptimizersHelp
    Help (Just Warnings) -> displayWarningsHelp
    Help (Just Target) -> displayTargetHelp
    Help (Just Compiler) -> displayCompilerHelp
    Version -> displayVersion
    Repl -> putStrLn "Launching REPL..."
    Compile file mode -> compileFile mode file

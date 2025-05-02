module Vex.CLI.Options where

data Command
  = Help (Maybe HelpTopic)
  | Version
  | Repl
  | Compile FilePath CompileMode

data CompileMode = EmitTokens | EmitAst | EmitIR | DefaultMode

data HelpTopic = General | Optimizers | Warnings | Target | Compiler

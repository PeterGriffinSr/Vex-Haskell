module Vex.CLI.Parser (parseArgs, Command (..), HelpTopic (..), CompileMode (..)) where

import Options.Applicative
import Vex.CLI.Options

parseArgs :: IO Command
parseArgs = execParser opts
  where
    opts =
      info
        (commandParser <**> helper)
        ( fullDesc
            <> progDesc "Vex Compiler"
            <> header "vex - the Vex programming language compiler"
        )

commandParser :: Parser Command
commandParser =
  subparser
    (command "repl" (info (pure Repl) (progDesc "Launch REPL")))
    <|> helpParser
    <|> versionParser
    <|> compileParser

helpParser :: Parser Command
helpParser =
  flag'
    (Help Nothing)
    (long "help" <> help "Display general help")
    <|> option
      (Help . Just <$> topicReader)
      ( long "help"
          <> metavar "TOPIC"
          <> help "Help for a specific topic (optimizers, warnings, target, compiler)"
      )

topicReader :: ReadM HelpTopic
topicReader = eitherReader $ \arg -> case arg of
  "optimizers" -> Right Optimizers
  "warnings" -> Right Warnings
  "target" -> Right Target
  "compiler" -> Right Compiler
  _ -> Left $ "Unknown help topic: " ++ arg

versionParser :: Parser Command
versionParser =
  flag'
    Version
    (long "version" <> help "Show compiler version")

compileParser :: Parser Command
compileParser =
  Compile
    <$> argument str (metavar "FILE")
    <*> compileModeParser

compileModeParser :: Parser CompileMode
compileModeParser =
  asum
    [ flag' EmitTokens (long "emit-tokens" <> help "Emit tokens"),
      flag' EmitAst (long "emit-ast" <> help "Emit AST"),
      flag' EmitIR (long "emit-ir" <> help "Emit IR"),
      pure DefaultMode
    ]

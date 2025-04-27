module Vex.Compiler.Compiler (handleArgs) where

import Control.Exception (IOException, catch)
import Control.Monad (when)
import Data.List (find, isPrefixOf, partition)
import qualified Data.Map as Map (Map, fromList, lookup, member)
import Data.Maybe (fromMaybe)
import System.Exit (exitFailure)
import Vex.CLI.Help (displayCompilerHelp, displayGeneral, displayOptimizersHelp, displayTargetHelp, displayVersion, displayWarningsHelp)
import Vex.Compiler.Lexer (lexer)
import Vex.Compiler.Parser (handleParseError, parseExpr)
import Vex.Core.AST (prettyExpr)
import Vex.Core.Error (noInputFile, unrecognizedFlag)

data CompileMode = EmitTokens | EmitAst | DefaultMode

isFlag :: String -> Bool
isFlag = ("--" `isPrefixOf`)

compileFile :: CompileMode -> FilePath -> IO ()
compileFile mode path = do
  result <- safeReadFile path
  result `orExit` \src -> do
    lexer path src src 1 1 `orExit` \tokens -> do
      case mode of
        EmitTokens -> do
          putStrLn "Tokens:"
          mapM_ print tokens
        _ -> return ()

      case mode of
        EmitAst -> do
          case parseExpr path src of
            Left parseErr -> handleParseError path src parseErr
            Right asts -> do
              putStrLn "Parsed expressions:"
              mapM_ (putStrLn . prettyExpr) asts
        _ -> return ()

handleString :: String -> IO ()
handleString arg
  | isFlag arg = putStrLn "Unrecognized flag"
  | otherwise = compileFile DefaultMode arg

safeReadFile :: FilePath -> IO (Either String String)
safeReadFile path = catch (Right <$> readFile path) handler
  where
    handler :: IOException -> IO (Either String String)
    handler e = return (Left $ "Error reading " ++ show e)

orExit :: Either String a -> (a -> IO ()) -> IO ()
orExit (Left err) _ = putStrLn err >> exitFailure
orExit (Right val) f = f val

handleArgs :: [String] -> IO ()
handleArgs args = do
  let (flags, files) = partition isFlag args
  case find (\f -> Map.member f commandTable && f /= "--emit-ast" && f /= "--emit-tokens") flags of
    Just flag -> do
      case Map.lookup flag commandTable of
        Just action -> action
        Nothing -> unrecognizedFlag flag
    Nothing -> do
      case find (\f -> not (Map.member f commandTable) && f /= "--emit-ast" && f /= "--emit-tokens") flags of
        Just unrecognized -> unrecognizedFlag unrecognized
        Nothing -> case files of
          [file] -> do
            let emitTokens = "--emit-tokens" `elem` flags
            let emitAst = "--emit-ast" `elem` flags
            if emitTokens
              then compileFile EmitTokens file
              else
                if emitAst
                  then compileFile EmitAst file
                  else compileFile DefaultMode file
          [] -> noInputFile
          _ -> do
            putStrLn "Error: Multiple files provided. Vex only supports one file at a time."
            exitFailure

prefixOf :: String -> String -> Bool
prefixOf = isPrefixOf

commandTable :: Map.Map String (IO ())
commandTable =
  Map.fromList
    [ ("--help", displayGeneral),
      ("--help=optimizers", displayOptimizersHelp),
      ("--help=warnings", displayWarningsHelp),
      ("--help=target", displayTargetHelp),
      ("--help=compiler", displayCompilerHelp),
      ("--version", displayVersion),
      ("repl", putStrLn "Launching REPL...")
    ]

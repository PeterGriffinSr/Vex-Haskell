module Compiler (handleArgs) where

import AST (prettyExpr)
import Control.Exception (IOException, catch)
import Control.Monad (when)
import Data.List (find, isPrefixOf, partition)
import qualified Data.Map as Map (Map, fromList, lookup, member)
import Data.Maybe (fromMaybe)
import Error (noInputFile, unrecognizedFlag)
import Help (displayCompilerHelp, displayGeneral, displayOptimizersHelp, displayTargetHelp, displayVersion, displayWarningsHelp)
import Lexer (lexer)
import Parser (handleParseError, parseExpr)
import System.Exit (exitFailure)

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
  case find (`Map.member` commandTable) flags of
    Just flag -> do
      fromMaybe (putStrLn "Unrecognized flag" >> exitFailure) (Map.lookup flag commandTable)
    Nothing -> case files of
      [file] -> do
        let emitTokens = "--emit-tokens" `elem` flags
        let emitAst = "--emit-ast" `elem` flags
        result <- safeReadFile file
        result `orExit` \src -> do
          when emitTokens $ do
            lexer file src src 1 1 `orExit` \tokens -> do
              putStrLn "Tokens:"
              mapM_ print tokens
          when emitAst $ do
            case parseExpr file src of
              Left parseErr -> handleParseError file src parseErr
              Right asts -> do
                putStrLn "Parsed expressions:"
                mapM_ (putStrLn . prettyExpr) asts
          when (not emitTokens && not emitAst) $
            compileFile DefaultMode file
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

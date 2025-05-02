module Vex.Compiler.Compiler where

import Control.Exception (IOException, catch)
import Control.Monad (when)
import Data.List (find, partition)
import Data.Maybe (fromMaybe)
import System.Exit (exitFailure)
import Vex.CLI.Options (CompileMode (..))
import Vex.Compiler.Lexer (lexer)
import Vex.Core.Error (noInputFile, unrecognizedFlag)

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

safeReadFile :: FilePath -> IO (Either String String)
safeReadFile path = catch (Right <$> readFile path) handler
  where
    handler :: IOException -> IO (Either String String)
    handler e = return (Left $ "Error reading " ++ show e)

orExit :: Either String a -> (a -> IO ()) -> IO ()
orExit (Left err) _ = putStrLn err >> exitFailure
orExit (Right val) f = f val

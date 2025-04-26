import Compiler (handleArgs)
import Error (noInputFile)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> noInputFile
    _ -> handleArgs args

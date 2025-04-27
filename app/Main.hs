import Compiler (handleArgs)
import Error (noInputFile)
import System.Environment (getArgs)
import Prelude (IO)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> noInputFile
    _ -> handleArgs args

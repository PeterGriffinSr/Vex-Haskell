import System.Environment (getArgs)
import Vex.Compiler.Compiler (handleArgs)
import Vex.Core.Error (noInputFile)
import Prelude (IO)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> noInputFile
    _ -> handleArgs args

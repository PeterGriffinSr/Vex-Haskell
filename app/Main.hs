import System.Environment (getArgs)
import Vex.CLI.Cli (handleArgs)
import Vex.Core.Error (noInputFile)
import Prelude (IO, null, (>>=))

main :: IO ()
main =
  getArgs >>= \args ->
    if null args
      then noInputFile
      else handleArgs

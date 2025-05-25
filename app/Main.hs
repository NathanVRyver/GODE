module Main where

import qualified Gode.Core as Gode
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [x, op, n, thenVal, elseVal] -> do
      let env = Gode.Env (read x) ("cli: " ++ x)
          program = Gode.If (unwords ["x", op, n]) thenVal elseVal
          (result, logs) = Gode.eval program env
      mapM_ putStrLn logs
      putStrLn $ "Result: " ++ result
    _ -> do
      putStrLn "Usage: gode <value> <operator> <threshold> <then-value> <else-value>"
      putStrLn "Example: gode 5 > 3 high low"
      putStrLn "Supported operators: >, <, =="

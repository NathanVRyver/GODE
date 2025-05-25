module Main where

import qualified Gode.Core as Gode
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let x = if null args then 4 else read (head args)
      env = Gode.Env x ("cli: " ++ show x)
      (result, logs) = Gode.eval Gode.program env
  mapM_ putStrLn logs
  putStrLn $ "Result: " ++ result

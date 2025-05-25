module Main where

import System.Environment (getArgs)
import qualified Main as Gode

main :: IO ()
main = do
  args <- getArgs
  let x = if null args then 4 else read (head args)
      env = Gode.Env x ("cli: " ++ show x)
      (result, logs) = Gode.eval Gode.program env
  mapM_ putStrLn logs
  putStrLn $ "Result: " ++ result

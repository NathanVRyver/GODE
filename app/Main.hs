module Main where

import qualified Gode.Core as Gode
import System.Environment (getArgs)
import Text.Read (readMaybe)

-- Parse input arguments
parseInput :: [String] -> Either String (Int, String, Int, String, String)
parseInput (xStr : op : yStr : thenVal : elseVal : _) = do
  x <- case readMaybe xStr of
    Just n -> Right n
    Nothing -> Left $ "Invalid number: " ++ xStr
  y <- case readMaybe yStr of
    Just n -> Right n
    Nothing -> Left $ "Invalid number: " ++ yStr
  Right (x, op, y, thenVal, elseVal)
parseInput _ = Left "Invalid number of arguments"

-- Format error message
formatError :: Gode.GodeError -> String
formatError (Gode.InvalidOperator op) = "Invalid operator: " ++ op
formatError (Gode.InvalidNumber num) = "Invalid number: " ++ num
formatError Gode.InvalidFormat = "Invalid condition format"

main :: IO ()
main = do
  args <- getArgs
  case parseInput args of
    Right (x, op, y, thenVal, elseVal) -> do
      let env = Gode.Env x ("cli: " ++ show x)
          program = Gode.If (unwords ["x", op, show y]) thenVal elseVal
      case Gode.eval program env of
        Right (result, logs) -> do
          mapM_ putStrLn logs
          putStrLn $ "Result: " ++ result
        Left err -> do
          putStrLn $ "Error: " ++ formatError err
          printUsage
    Left err -> do
      putStrLn $ "Error: " ++ err
      printUsage

printUsage :: IO ()
printUsage = do
  putStrLn "\nUsage: gode <value> <operator> <threshold> <then-value> <else-value>"
  putStrLn "Example: gode 5 '>' 3 high low"
  putStrLn "Example: gode 2 '<' 3 low high"
  putStrLn "Example: gode 3 '==' 3 match no-match"
  putStrLn "Supported operators: '>', '<', '=='"

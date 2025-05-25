module Main where

import qualified Gode.Core as Gode
import System.Console.ANSI (Color (..), ConsoleLayer (..), SGR (..), setSGR)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
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

-- Print colored error message
printError :: String -> IO ()
printError msg = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn msg
  setSGR [Reset]

-- Interactive REPL
repl :: IO ()
repl = do
  putStr "gode> " >> hFlush stdout
  input <- getLine
  case words input of
    ["quit"] -> pure ()
    [x, op, y, thenVal, elseVal] ->
      case parseInput [x, op, y, thenVal, elseVal] of
        Right (xVal, opStr, yVal, tVal, eVal) -> do
          let env = Gode.Env xVal ("repl: " ++ show xVal)
              program = Gode.If (unwords ["x", opStr, show yVal]) tVal eVal
          case Gode.eval program env of
            Right (result, logs) -> do
              mapM_ putStrLn logs
              putStrLn $ "Result: " ++ result
            Left err -> printError $ "Error: " ++ formatError err
        Left err -> printError $ "Error: " ++ err
    _ -> printError "Usage: <x> <op> <y> <then> <else>"
  repl

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["repl"] -> do
      putStrLn "GODE Interactive Mode (type 'quit' to exit)"
      putStrLn "Example: 5 '>' 3 high low"
      putStrLn "Supported operators: '>', '<', '==', '>=', '<='"
      repl
    _ -> case parseInput args of
      Right (x, op, y, thenVal, elseVal) -> do
        let env = Gode.Env x ("cli: " ++ show x)
            program = Gode.If (unwords ["x", op, show y]) thenVal elseVal
        case Gode.eval program env of
          Right (result, logs) -> do
            mapM_ putStrLn logs
            putStrLn $ "Result: " ++ result
          Left err -> printError $ "Error: " ++ formatError err
      Left err -> do
        printError $ "Error: " ++ err
        printUsage

printUsage :: IO ()
printUsage = do
  putStrLn "\nUsage: gode <value> <operator> <threshold> <then-value> <else-value>"
  putStrLn "Example: gode 5 '>' 3 high low"
  putStrLn "Example: gode 2 '<' 3 low high"
  putStrLn "Example: gode 3 '==' 3 match no-match"
  putStrLn "Example: gode 3 '>=' 3 high low"
  putStrLn "Example: gode 2 '<=' 3 low high"
  putStrLn "Supported operators: '>', '<', '==', '>=', '<='"
  putStrLn "\nOr start interactive mode:"
  putStrLn "gode repl"

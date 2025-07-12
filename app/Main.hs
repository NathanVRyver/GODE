module Main where

import qualified Gode.Core as Gode
import qualified Gode.Parser as Parser
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), setSGR)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import qualified Data.Map as Map
import Data.List (isPrefixOf)

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
formatError (Gode.UndefinedVariable var) = "Undefined variable: " ++ var
formatError (Gode.TypeError msg) = "Type error: " ++ msg
formatError Gode.DivisionByZero = "Division by zero"
formatError (Gode.ParseError msg) = "Parse error: " ++ msg

-- Print colored error message
printError :: String -> IO ()
printError msg = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn msg
  setSGR [Reset]

-- Parse variable definition (e.g., "x=5")
parseVarDef :: String -> Maybe (String, Gode.Value)
parseVarDef s = case break (== '=') s of
  (var, '=':val) -> case readMaybe val of
    Just n -> Just (var, Gode.IntVal n)
    Nothing -> if val == "true" then Just (var, Gode.BoolVal True)
               else if val == "false" then Just (var, Gode.BoolVal False)
               else Just (var, Gode.StringVal val)
  _ -> Nothing

-- Interactive REPL
repl :: IO ()
repl = replWithEnv Map.empty

replWithEnv :: Gode.Environment -> IO ()
replWithEnv env = do
  putStr "gode> " >> hFlush stdout
  input <- getLine
  case input of
    "quit" -> pure ()
    "vars" -> do
      if Map.null env
        then putStrLn "No variables defined"
        else do
          putStrLn "Current variables:"
          mapM_ (\(name, (val, src)) -> 
            putStrLn $ "  " ++ name ++ " = " ++ Gode.valueToString val ++ " (from " ++ src ++ ")")
            (Map.toList env)
      replWithEnv env
    _ | "let " `isPrefixOf` input -> do
      let varDefs = words (drop 4 input)
      case mapM parseVarDef varDefs of
        Just defs -> do
          let newEnv = foldl (\e (name, val) -> Map.insert name (val, "repl") e) env defs
          putStrLn $ "Defined: " ++ unwords (map fst defs)
          replWithEnv newEnv
        Nothing -> do
          printError "Invalid variable definition. Use: let x=5 y=10"
          replWithEnv env
    _ -> 
      -- Try simple format first for backward compatibility
      case words input of
        [x, op, y, thenVal, elseVal] ->
          case parseInput [x, op, y, thenVal, elseVal] of
            Right (xVal, opStr, yVal, tVal, eVal) -> do
              case Gode.evalSimple x opStr (show yVal) thenVal elseVal of
                Right (result, logs) -> do
                  mapM_ putStrLn logs
                  putStrLn $ "Result: " ++ result
                Left err -> printError $ "Error: " ++ formatError err
              replWithEnv env
            Left _ -> evalComplexExpr input env
        _ -> evalComplexExpr input env
  where
    evalComplexExpr expr env' = do
      case Parser.parse expr of
        Right ast -> do
          case Gode.eval ast env' of
            Right (result, logs) -> do
              mapM_ putStrLn logs
              putStrLn $ "Result: " ++ Gode.valueToString result
            Left err -> printError $ "Error: " ++ formatError err
        Left err -> printError $ "Error: " ++ formatError err
      replWithEnv env'

-- Process a file with expressions
processFile :: String -> IO ()
processFile filename = do
  contents <- readFile filename
  case parseFileContents contents of
    Left err -> printError $ "Error: " ++ err
    Right commands -> do
      putStrLn $ "Processing file: " ++ filename
      putStrLn ""
      processCommands commands Map.empty
  where
    processCommands [] _ = return ()
    processCommands (cmd:cmds) env = do
      (newEnv, shouldContinue) <- processCommand cmd env
      if shouldContinue 
        then processCommands cmds newEnv
        else return ()

-- Process a single command from file
processCommand :: FileCommand -> Gode.Environment -> IO (Gode.Environment, Bool)
processCommand cmd env = case cmd of
  VarDefCommand name value -> do
    let newEnv = Map.insert name (value, "file") env
    putStrLn $ "Defined: " ++ name ++ " = " ++ Gode.valueToString value
    return (newEnv, True)
  
  ExprCommand expr -> do
    case Parser.parse expr of
      Right ast -> do
        case Gode.eval ast env of
          Right (result, logs) -> do
            mapM_ putStrLn logs
            putStrLn $ "Result: " ++ Gode.valueToString result
            putStrLn ""
            return (env, True)
          Left err -> do
            printError $ "Error: " ++ formatError err
            return (env, False)
      Left err -> do
        printError $ "Error: " ++ formatError err
        return (env, False)

-- File command types
data FileCommand
  = VarDefCommand String Gode.Value
  | ExprCommand String

-- Parse a single line from file
parseFileLine :: String -> Either String [FileCommand]
parseFileLine line
  | "let " `isPrefixOf` line = 
      let varDefs = words (drop 4 line)
      in case mapM parseVarDef varDefs of
        Just defs -> Right $ map (\(name, value) -> VarDefCommand name value) defs
        Nothing -> Left $ "Invalid variable definition: " ++ line
  | otherwise = Right [ExprCommand line]

-- Parse file contents into commands
parseFileContents :: String -> Either String [FileCommand]
parseFileContents contents = do
  let nonEmptyLines = filter (not . null) . map (dropWhile (== ' ')) . lines $ contents
      nonCommentLines = filter (not . ("--" `isPrefixOf`)) nonEmptyLines
  commandLists <- mapM parseFileLine nonCommentLines
  return $ concat commandLists

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--file", filename] -> processFile filename
    ["-f", filename] -> processFile filename
    ["repl"] -> do
      putStrLn "GODE Interactive Mode (type 'quit' to exit)"
      putStrLn "\nFeatures:"
      putStrLn "  - Simple mode: <number> <operator> <number> <then-value> <else-value>"
      putStrLn "  - Complex expressions: if x > 5 && y < 10 then \"high\" else \"low\""
      putStrLn "  - Variable definitions: let x=5 y=10"
      putStrLn "  - Show variables: vars"
      putStrLn "\nExamples:"
      putStrLn "  2 < 6 high low"
      putStrLn "  let x=5 y=3"
      putStrLn "  if x > y then \"x is bigger\" else \"y is bigger\""
      putStrLn "  x * 2 + y"
      putStrLn "\nSupported operators: +, -, *, /, >, <, ==, !=, >=, <=, &&, ||, not"
      repl
    _ -> case parseInput args of
      Right (x, op, y, thenVal, elseVal) -> do
        case Gode.evalSimple (show x) op (show y) thenVal elseVal of
          Right (result, logs) -> do
            mapM_ putStrLn logs
            putStrLn $ "Result: " ++ result
          Left err -> printError $ "Error: " ++ formatError err
      Left err -> do
        printError $ "Error: " ++ err
        printUsage

printUsage :: IO ()
printUsage = do
  putStrLn "\nUsage:"
  putStrLn "  gode <value> <operator> <threshold> <then-value> <else-value>"
  putStrLn "  gode repl"
  putStrLn "  gode --file <filename>"
  putStrLn "  gode -f <filename>"
  putStrLn "\nExamples:"
  putStrLn "  gode 5 '>' 3 high low"
  putStrLn "  gode 2 '<' 3 low high"
  putStrLn "  gode repl"
  putStrLn "  gode --file examples/debug.gode"
  putStrLn "\nFile format example (debug.gode):"
  putStrLn "  -- Define variables"
  putStrLn "  let x=10 y=5"
  putStrLn "  -- Evaluate expressions"
  putStrLn "  x + y"
  putStrLn "  if x > y then \"x is bigger\" else \"y is bigger\""
  putStrLn "  x * 2 - y"

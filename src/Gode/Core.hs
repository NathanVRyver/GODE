module Gode.Core where

import Text.Read (readMaybe)

-- Environment holds value and its origin
data Env = Env {val :: Int, src :: String}

-- Tiny AST: if (x > n) then "A" else "B"
data Expr = If String String String

-- Supported operators
data Operator = GreaterThan | LessThan | Equals
  deriving (Show, Eq)

-- Error types
data GodeError
  = InvalidOperator String
  | InvalidNumber String
  | InvalidFormat
  deriving (Show, Eq)

-- Parse operator from string
parseOperator :: String -> Either GodeError Operator
parseOperator ">" = Right GreaterThan
parseOperator "<" = Right LessThan
parseOperator "==" = Right Equals
parseOperator op = Left $ InvalidOperator op

-- Parse number from string
parseNumber :: String -> Either GodeError Int
parseNumber str = case readMaybe str of
  Just n -> Right n
  Nothing -> Left $ InvalidNumber str

-- Evaluate and log decisions
eval :: Expr -> Env -> Either GodeError (String, [String])
eval (If cond thenVal elseVal) env =
  case checkCond cond env of
    Right (condMet, reason) ->
      let result = if condMet then thenVal else elseVal
          logs =
            [ "[GODE] Decision: " ++ cond ++ " is " ++ show condMet,
              "[GODE]   ↳ val = " ++ show (val env) ++ " (from " ++ src env ++ ")",
              "[GODE]   ↳ Because " ++ reason
            ]
       in Right (result, logs)
    Left err -> Left err

-- Check condition with support for multiple operators
checkCond :: String -> Env -> Either GodeError (Bool, String)
checkCond cond env =
  case words cond of
    ["x", op, n] -> do
      operator <- parseOperator op
      threshold <- parseNumber n
      let (met, desc) = case operator of
            GreaterThan -> (val env > threshold, ">")
            LessThan -> (val env < threshold, "<")
            Equals -> (val env == threshold, "==")
      Right (met, show (val env) ++ " " ++ desc ++ " " ++ n)
    _ -> Left InvalidFormat

-- Example program
program :: Expr
program = If "x > 3" "high" "low"

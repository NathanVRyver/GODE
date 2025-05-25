module Gode.Core where

-- Environment holds value and its origin
data Env = Env {val :: Int, src :: String}

-- Tiny AST: if (x > n) then "A" else "B"
data Expr = If String String String

-- Supported operators
data Operator = GreaterThan | LessThan | Equals
  deriving (Show, Eq)

-- Parse operator from string
parseOperator :: String -> Maybe Operator
parseOperator ">" = Just GreaterThan
parseOperator "<" = Just LessThan
parseOperator "==" = Just Equals
parseOperator _ = Nothing

-- Evaluate and log decisions
eval :: Expr -> Env -> (String, [String])
eval (If cond thenVal elseVal) env =
  let (condMet, reason) = checkCond cond env
      result = if condMet then thenVal else elseVal
      logs =
        [ "[GODE] Decision: " ++ cond ++ " is " ++ show condMet,
          "[GODE]   ↳ val = " ++ show (val env) ++ " (from " ++ src env ++ ")",
          "[GODE]   ↳ Because " ++ reason
        ]
   in (result, logs)

-- Check condition with support for multiple operators
checkCond :: String -> Env -> (Bool, String)
checkCond cond env =
  case words cond of
    ["x", op, n] ->
      case parseOperator op of
        Just operator ->
          let threshold = read n
              (met, desc) = case operator of
                GreaterThan -> (val env > threshold, ">")
                LessThan -> (val env < threshold, "<")
                Equals -> (val env == threshold, "==")
           in (met, show (val env) ++ " " ++ desc ++ " " ++ n)
        Nothing -> error $ "Unsupported operator: " ++ op
    _ -> error "Unsupported condition format"

-- Example program
program :: Expr
program = If "x > 3" "high" "low"

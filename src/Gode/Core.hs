module Gode.Core where

-- Environment holds value and its origin
data Env = Env {val :: Int, src :: String}

-- Tiny AST: if (x > n) then "A" else "B"
data Expr = If String String String

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

-- Check condition (only supports "x > n")
checkCond :: String -> Env -> (Bool, String)
checkCond cond env =
  case words cond of
    ["x", ">", n] ->
      let threshold = read n
       in (val env > threshold, show (val env) ++ " > " ++ n)
    _ -> error "Unsupported condition"

-- Example program
program :: Expr
program = If "x > 3" "high" "low"

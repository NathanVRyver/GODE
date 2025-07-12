module Gode.Core where

import Text.Read (readMaybe)
import qualified Data.Map as Map
import Data.Map (Map)

-- Environment holds multiple variables with their values and origins
type VarName = String
type VarValue = Value
type VarSource = String
type Environment = Map VarName (VarValue, VarSource)

-- Values can be of different types
data Value
  = IntVal Int
  | StringVal String
  | BoolVal Bool
  deriving (Show, Eq)

-- Enhanced AST for complex expressions
data Expr
  = Literal Value
  | Var VarName
  | BinOp BinOperator Expr Expr
  | UnaryOp UnaryOperator Expr
  | If Expr Expr Expr
  deriving (Show, Eq)

-- Binary operators
data BinOperator
  = -- Comparison
    GreaterThan
  | LessThan
  | Equals
  | NotEquals
  | GreaterThanOrEqual
  | LessThanOrEqual
  | -- Boolean
    And
  | Or
  | -- Arithmetic
    Add
  | Sub
  | Mul
  | Div
  deriving (Show, Eq)

-- Unary operators
data UnaryOperator
  = Not
  | Negate
  deriving (Show, Eq)

-- Error types
data GodeError
  = InvalidOperator String
  | InvalidNumber String
  | InvalidFormat
  | UndefinedVariable VarName
  | TypeError String
  | DivisionByZero
  | ParseError String
  deriving (Show, Eq)

-- Helper functions for value operations
valueToString :: Value -> String
valueToString (IntVal n) = show n
valueToString (StringVal s) = s
valueToString (BoolVal b) = show b

-- Evaluate expression with tracing
eval :: Expr -> Environment -> Either GodeError (Value, [String])
eval expr env = evalWithTrace expr env []

evalWithTrace :: Expr -> Environment -> [String] -> Either GodeError (Value, [String])
evalWithTrace expr env logs = case expr of
  Literal v -> Right (v, logs)
  
  Var name -> case Map.lookup name env of
    Just (val, src) -> 
      let logEntry = "[GODE] Variable '" ++ name ++ "' = " ++ valueToString val ++ " (from " ++ src ++ ")"
      in Right (val, logs ++ [logEntry])
    Nothing -> Left $ UndefinedVariable name
  
  BinOp op left right -> do
    (lval, llogs) <- evalWithTrace left env logs
    (rval, rlogs) <- evalWithTrace right env llogs
    (result, opLog) <- evalBinOp op lval rval
    Right (result, rlogs ++ [opLog])
  
  UnaryOp op expr' -> do
    (val, elogs) <- evalWithTrace expr' env logs
    (result, opLog) <- evalUnaryOp op val
    Right (result, elogs ++ [opLog])
  
  If cond thenExpr elseExpr -> do
    (condVal, clogs) <- evalWithTrace cond env logs
    case condVal of
      BoolVal True -> do
        let decisionLog = "[GODE] Decision: condition is True, taking 'then' branch"
        (result, tlogs) <- evalWithTrace thenExpr env (clogs ++ [decisionLog])
        Right (result, tlogs)
      BoolVal False -> do
        let decisionLog = "[GODE] Decision: condition is False, taking 'else' branch"
        (result, elogs) <- evalWithTrace elseExpr env (clogs ++ [decisionLog])
        Right (result, elogs)
      _ -> Left $ TypeError "If condition must be boolean"

-- Evaluate binary operations
evalBinOp :: BinOperator -> Value -> Value -> Either GodeError (Value, String)
evalBinOp op lval rval = case (op, lval, rval) of
  -- Arithmetic operations
  (Add, IntVal a, IntVal b) -> 
    Right (IntVal (a + b), "[GODE] Operation: " ++ show a ++ " + " ++ show b ++ " = " ++ show (a + b))
  (Sub, IntVal a, IntVal b) -> 
    Right (IntVal (a - b), "[GODE] Operation: " ++ show a ++ " - " ++ show b ++ " = " ++ show (a - b))
  (Mul, IntVal a, IntVal b) -> 
    Right (IntVal (a * b), "[GODE] Operation: " ++ show a ++ " * " ++ show b ++ " = " ++ show (a * b))
  (Div, IntVal _, IntVal 0) -> Left DivisionByZero
  (Div, IntVal a, IntVal b) -> 
    Right (IntVal (a `div` b), "[GODE] Operation: " ++ show a ++ " / " ++ show b ++ " = " ++ show (a `div` b))
  
  -- Comparison operations
  (GreaterThan, IntVal a, IntVal b) -> 
    Right (BoolVal (a > b), "[GODE] Comparison: " ++ show a ++ " > " ++ show b ++ " = " ++ show (a > b))
  (LessThan, IntVal a, IntVal b) -> 
    Right (BoolVal (a < b), "[GODE] Comparison: " ++ show a ++ " < " ++ show b ++ " = " ++ show (a < b))
  (GreaterThanOrEqual, IntVal a, IntVal b) -> 
    Right (BoolVal (a >= b), "[GODE] Comparison: " ++ show a ++ " >= " ++ show b ++ " = " ++ show (a >= b))
  (LessThanOrEqual, IntVal a, IntVal b) -> 
    Right (BoolVal (a <= b), "[GODE] Comparison: " ++ show a ++ " <= " ++ show b ++ " = " ++ show (a <= b))
  (Equals, a, b) -> 
    Right (BoolVal (a == b), "[GODE] Comparison: " ++ valueToString a ++ " == " ++ valueToString b ++ " = " ++ show (a == b))
  (NotEquals, a, b) -> 
    Right (BoolVal (a /= b), "[GODE] Comparison: " ++ valueToString a ++ " != " ++ valueToString b ++ " = " ++ show (a /= b))
  
  -- Boolean operations
  (And, BoolVal a, BoolVal b) -> 
    Right (BoolVal (a && b), "[GODE] Boolean: " ++ show a ++ " && " ++ show b ++ " = " ++ show (a && b))
  (Or, BoolVal a, BoolVal b) -> 
    Right (BoolVal (a || b), "[GODE] Boolean: " ++ show a ++ " || " ++ show b ++ " = " ++ show (a || b))
  
  -- Type errors
  _ -> Left $ TypeError "Type mismatch in binary operation"

-- Evaluate unary operations
evalUnaryOp :: UnaryOperator -> Value -> Either GodeError (Value, String)
evalUnaryOp op val = case (op, val) of
  (Not, BoolVal b) -> 
    Right (BoolVal (not b), "[GODE] Boolean: NOT " ++ show b ++ " = " ++ show (not b))
  (Negate, IntVal n) -> 
    Right (IntVal (-n), "[GODE] Arithmetic: -(" ++ show n ++ ") = " ++ show (-n))
  _ -> Left $ TypeError "Type mismatch in unary operation"

-- Legacy compatibility function for simple expressions
evalSimple :: String -> String -> String -> String -> String -> Either GodeError (String, [String])
evalSimple valStr op thresholdStr thenVal elseVal = do
  value <- case readMaybe valStr of
    Just n -> Right n
    Nothing -> Left $ InvalidNumber valStr
  threshold <- case readMaybe thresholdStr of
    Just n -> Right n
    Nothing -> Left $ InvalidNumber thresholdStr
  
  let env = Map.singleton "x" (IntVal value, "command-line")
  binOp <- case op of
    ">" -> Right GreaterThan
    "<" -> Right LessThan
    "==" -> Right Equals
    ">=" -> Right GreaterThanOrEqual
    "<=" -> Right LessThanOrEqual
    _ -> Left $ InvalidOperator op
  
  let expr = If (BinOp binOp (Var "x") (Literal (IntVal threshold))) 
                (Literal (StringVal thenVal)) 
                (Literal (StringVal elseVal))
  
  (result, logs) <- eval expr env
  case result of
    StringVal s -> Right (s, logs)
    _ -> Left $ TypeError "Expected string result"

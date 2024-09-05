module APL.Eval
  (
    Val(..),
    eval,
    envEmpty
  )
where

import APL.AST (Exp (..), VName)
import Data.Maybe (fromMaybe)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend vName val env = (vName, val) : env

-- | Look up a variable name in the provided environment.
-- Returns Nothing if the variable is not in the environment.
envLookup :: VName -> Env -> Maybe Val
envLookup vName env = lookup vName env

data Val
  = ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)

type Error = String

eval :: Env -> Exp -> Either Error Val
eval env (CstInt x) = Right $ ValInt x
eval env (Add x y) = 
  case (eval env x, eval env y) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x + y
    (_, Right y) -> Left "Error: Cannot add x and y together, since x doesn't have type Val"
    (Right x, _) -> Left "Error: Cannot add x and y together, since y doesn't have type Val"
    _ -> Left "Error: Cannot add x and y together, since neither value has type Val"

eval env (Sub x y) = 
  case (eval env x, eval env y) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x - y
    (_, Right y) -> Left "Error: Cannot sub x and y together, since x doesn't have type Val"
    (Right x, _) -> Left "Error: Cannot sub x and y together, since y doesn't have type Val"
    _ -> Left "Error: Cannot sub x and y together, since neither value has type Val"

eval env (Mul x y) = 
  case (eval env x, eval env y) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x * y
    (_, Right y) -> Left "Error: Cannot mul x and y together, since x doesn't have type Val"
    (Right x, _) -> Left "Error: Cannot mul x and y together, since y doesn't have type Val"
    _ -> Left "Error: Cannot mul x and y together, since neither value has type Val"

eval env (Div x y) = 
  case (eval env x, eval env y) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt 0)) -> Left "Error: Division by zero is not supported"
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x `div` y
    (_, Right y) -> Left "Error: Cannot div x and y together, since x doesn't have type Val"
    (Right x, _) -> Left "Error: Cannot div x and y together, since y doesn't have type Val"
    _ -> Left "Error: Cannot div x and y together, since neither value has type Val"

eval env (Pow x y) = 
  case (eval env x, eval env y) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) 
      | y < 0 -> Left "Error: Negative exponents are not supported"
      | otherwise -> Right $ ValInt $ x ^ y
    (_, Right y) -> Left "Error: Cannot pow x and y together, since x doesn't have type Val"
    (Right x, _) -> Left "Error: Cannot pow x and y together, since y doesn't have type Val"
    _ -> Left "Error: Cannot pow x and y together, since neither value has type Val"

eval env (CstBool x) = Right $ ValBool x

eval env (Eql x y) = 
  case (eval env x, eval env y) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right (ValBool (x == y)) 
    (Right (ValBool x), Right (ValBool y)) -> Right (ValBool (x == y)) 
    (Right _, Right _) -> Left "Error: Cannot compare x and y since they have different types"
    _ -> Left "Error: Cannot pow x and y together, since neither value has type Val"

eval env (If cond x y) = 
  case (eval env cond) of
    Left err -> Left err
    Right (ValBool True) -> eval env x
    Right (ValBool False) -> eval env y
    Right _ -> Left "Error: Condition is not a boolean"

eval env (Var x) = 
  case (envLookup x env) of
    Nothing -> Left "Error: Variable not found in environment"
    Just v -> Right v

eval env (Let vName expr1 expr2) = 
  case (eval env expr1) of
    Left err -> Left err
    Right v -> eval (envExtend vName v env) expr2

  


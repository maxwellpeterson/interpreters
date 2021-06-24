module Core.ExprC
  ( ExprC (..),
    FunDefC (..),
    InterpError (..),
    interp,
  )
where

import Data.List (find)

-- Expressions with Function Application

data ExprC = Value Int | Add ExprC ExprC | Mul ExprC ExprC | AppC Name ExprC | IdC Identifier

-- Function Definitions

data FunDefC = FunDefC Name Identifier ExprC

type Name = String

type Identifier = String

-- Possible Error States

data InterpError = UndefinedFunction Name | UnboundIdentifier Identifier deriving (Eq, Show)

type InterpResult = Either InterpError

interp :: ExprC -> [FunDefC] -> InterpResult Int
interp (Value num) funDefs = return num
interp (Add left right) funDefs = (+) <$> interp left funDefs <*> interp right funDefs
interp (Mul left right) funDefs = (*) <$> interp left funDefs <*> interp right funDefs
interp (AppC name inputExpr) funDefs = case find (\(FunDefC nn _ _) -> nn == name) funDefs of
  Nothing -> Left (UndefinedFunction name)
  Just (FunDefC _ identifier body) -> do
    inputValue <- interp inputExpr funDefs
    interp (subst inputValue identifier body) funDefs
interp (IdC identifier) funDefs = Left (UnboundIdentifier identifier)

subst :: Int -> Identifier -> ExprC -> ExprC
subst value targetIdentifier = substExpr
  where
    substExpr v@(Value _) = v
    substExpr (Add left right) = Add (substExpr left) (substExpr right)
    substExpr (Mul left right) = Mul (substExpr left) (substExpr right)
    substExpr (AppC name inputExpr) = AppC name (substExpr inputExpr)
    substExpr id@(IdC identifier)
      | identifier == targetIdentifier = Value value
      | otherwise = id
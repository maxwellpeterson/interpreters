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

interp :: [FunDefC] -> ExprC -> InterpResult Int
interp funDefs = interpExpr
  where
    interpExpr (Value num) = return num
    interpExpr (Add left right) = (+) <$> interpExpr left <*> interpExpr right
    interpExpr (Mul left right) = (*) <$> interpExpr left <*> interpExpr right
    interpExpr (AppC name inputExpr) = case lookup name of
      Nothing -> Left (UndefinedFunction name)
      Just (FunDefC _ identifier body) -> do
        inputValue <- interpExpr inputExpr
        interpExpr (subst inputValue identifier body)
    interpExpr (IdC identifier) = Left (UnboundIdentifier identifier)
    lookup targetName = find (\(FunDefC name _ _) -> name == targetName) funDefs

subst :: Int -> Identifier -> ExprC -> ExprC
subst value targetIdentifier = substExpr
  where
    substExpr (Add left right) = Add (substExpr left) (substExpr right)
    substExpr (Mul left right) = Mul (substExpr left) (substExpr right)
    substExpr (AppC name inputExpr) = AppC name (substExpr inputExpr)
    substExpr (IdC identifier) | identifier == targetIdentifier = Value value
    substExpr unchanged = unchanged -- Is this a bad pattern?
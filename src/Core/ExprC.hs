module Core.ExprC
  ( ExprC (..),
    FunDefC (..),
    Name,
    Identifier,
    InterpError (..),
    InterpResult,
    interp,
    subst,
  )
where

import Data.List (find, lookup)

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
    interpExpr :: ExprC -> InterpResult Int
    interpExpr (Value num) = return num
    interpExpr (Add left right) = (+) <$> interpExpr left <*> interpExpr right
    interpExpr (Mul left right) = (*) <$> interpExpr left <*> interpExpr right
    interpExpr (AppC name inputExpr) = case lookupFunDef name of
      Nothing -> Left (UndefinedFunction name)
      Just (FunDefC _ identifier body) -> do
        inputValue <- interpExpr inputExpr
        interpExpr (subst inputValue identifier body)
    interpExpr (IdC identifier) = Left (UnboundIdentifier identifier)
    lookupFunDef :: Name -> Maybe FunDefC
    lookupFunDef targetName = find (\(FunDefC name _ _) -> name == targetName) funDefs

subst :: Int -> Identifier -> ExprC -> ExprC
subst value targetIdentifier = substExpr
  where
    substExpr (Add left right) = Add (substExpr left) (substExpr right)
    substExpr (Mul left right) = Mul (substExpr left) (substExpr right)
    -- Check that function body should be untouched here. I think so, since
    -- function is defined in a separate scope...
    substExpr (AppC name inputExpr) = AppC name (substExpr inputExpr)
    substExpr (IdC identifier) | identifier == targetIdentifier = Value value
    substExpr unchanged = unchanged -- Is this a bad pattern?

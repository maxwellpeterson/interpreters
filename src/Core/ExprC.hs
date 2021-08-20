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

-- Expressions with function application

data ExprC a
  = Value a
  | Add (ExprC a) (ExprC a)
  | Mul (ExprC a) (ExprC a)
  | AppC Name (ExprC a)
  | IdC Identifier
  deriving (Show)

-- Function definitions

data FunDefC a = FunDefC Name Identifier (ExprC a) deriving (Show)

type Name = String

type Identifier = String

-- Possible error states

data InterpError = UndefinedFunction Name | UnboundIdentifier Identifier deriving (Eq, Show)

type InterpResult = Either InterpError

interp :: Num a => [FunDefC a] -> ExprC a -> InterpResult a
interp funDefs = interpExpr
  where
    -- Need scoped type variables in order to add signature here?
    -- interpExpr :: ExprC a -> Either InterpError a
    interpExpr (Value num) = return num
    interpExpr (Add left right) = (+) <$> interpExpr left <*> interpExpr right
    interpExpr (Mul left right) = (*) <$> interpExpr left <*> interpExpr right
    interpExpr (AppC name inputExpr) = case lookupFunDef name of
      Nothing -> Left (UndefinedFunction name)
      Just (FunDefC _ identifier body) -> do
        inputValue <- interpExpr inputExpr
        interpExpr (subst inputValue identifier body)
    interpExpr (IdC identifier) = Left (UnboundIdentifier identifier)
    -- lookupFunDef :: Name -> Maybe (FunDefC a)
    lookupFunDef targetName = find (\(FunDefC name _ _) -> name == targetName) funDefs

subst :: a -> Identifier -> ExprC a -> ExprC a
subst value targetIdentifier = substExpr
  where
    substExpr (Add left right) = Add (substExpr left) (substExpr right)
    substExpr (Mul left right) = Mul (substExpr left) (substExpr right)
    substExpr (AppC name inputExpr) = AppC name (substExpr inputExpr)
    substExpr (IdC identifier) | identifier == targetIdentifier = Value value
    substExpr unchanged = unchanged -- Is this a bad pattern?

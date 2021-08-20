module Core.Deferred (Environment, Binding, interp) where

import qualified Core.ExprC as E
import Data.List (find)

-- Structures for Deferred Substitution

type Environment a = [Binding a]

type Binding a = (E.Identifier, a)

interp :: Num a => [E.FunDefC a] -> E.ExprC a -> E.InterpResult a
interp funDefs = interpEnv []
  where
    -- Same situation with scoped type variables...
    -- interpEnv :: Environment a -> E.ExprC a -> E.InterpResult a
    interpEnv _ (E.Value num) = return num
    interpEnv env (E.Add left right) = (+) <$> interpEnv env left <*> interpEnv env right
    interpEnv env (E.Mul left right) = (*) <$> interpEnv env left <*> interpEnv env right
    interpEnv env (E.AppC name inputExpr) = case lookupFunDef name of
      Nothing -> Left (E.UndefinedFunction name)
      Just (E.FunDefC _ identifier body) -> do
        inputValue <- interpEnv env inputExpr
        -- Environment gets reset on function application
        interpEnv [(identifier, inputValue)] body
    interpEnv env (E.IdC identifier) = case lookup identifier env of
      Nothing -> Left (E.UnboundIdentifier identifier)
      Just num -> return num
    -- lookupFunDef :: E.Name -> Maybe (E.FunDefC a)
    lookupFunDef targetName = find (\(E.FunDefC name _ _) -> name == targetName) funDefs
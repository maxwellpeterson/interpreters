module Core.LamExprC
  ( LamExprC (..),
    Closure (..),
    InterpError (..),
    InterpResult (..),
    interp,
  )
where

import qualified Core.ExprC as E

data LamExprC a
  = Value a
  | Add (LamExprC a) (LamExprC a)
  | Mul (LamExprC a) (LamExprC a)
  | AppC (LamExprC a) (LamExprC a)
  | IdC E.Identifier
  | LamC E.Identifier (LamExprC a)
  deriving (Eq, Show)

-- Lazy bindings, where expressions may evaluate to values or closures (or
-- errors). Lazy is convenient, since it eliminates the need for custom error
-- handling when evaluating function argument expressions, as well as the need
-- for a new "value or closure" type that gets stored by the environment.

type Environment a = [(E.Identifier, LamExprC a)]

data Closure a = Closure E.Identifier (LamExprC a) (Environment a) deriving (Eq, Show)

data InterpError
  = UnboundIdentifier E.Identifier
  | UnexpectedValue
  | UnexpectedClosure
  deriving (Eq, Show)

data InterpResult a
  = ValueResult a
  | ClosureResult (Closure a)
  | ErrorResult InterpError
  deriving (Eq, Show)

interp :: Num a => LamExprC a -> InterpResult a
interp = interpEnv []

interpEnv :: forall a. Num a => Environment a -> LamExprC a -> InterpResult a
interpEnv env = interpCurEnv
  where
    interpCurEnv :: LamExprC a -> InterpResult a
    interpCurEnv (Value num) = ValueResult num
    interpCurEnv (Add left right) = interpOperator (+) left right
    interpCurEnv (Mul left right) = interpOperator (*) left right
    interpCurEnv (AppC func inputExpr) = case interpCurEnv func of
      -- Replace current environment with closure environment plus input
      -- expression binding on application
      ClosureResult (Closure identifier body closureEnv) -> interpEnv (
        (identifier, inputExpr) : closureEnv
        ) body
      ValueResult _ -> ErrorResult UnexpectedValue
      (ErrorResult error) -> ErrorResult error
    interpCurEnv (IdC identifier) = case lookup identifier env of
      -- Binding expression gets evaluated in current (closure) environment
      Just expr -> interpCurEnv expr
      Nothing -> ErrorResult (UnboundIdentifier identifier)
    -- Closure captures current environment
    interpCurEnv (LamC identifier body) = ClosureResult (Closure identifier body env)
    interpOperator :: (a -> a -> a) -> LamExprC a -> LamExprC a -> InterpResult a
    interpOperator operator left right = wrapResult (
      operator <$> (checkResult . interpCurEnv $ left)
      <*> (checkResult . interpCurEnv $ right)
      )

checkResult :: Num a => InterpResult a -> Either InterpError a
checkResult (ValueResult num) = Right num
checkResult (ClosureResult _) = Left UnexpectedClosure
checkResult (ErrorResult error) = Left error

wrapResult :: Either InterpError a -> InterpResult a
wrapResult (Left error) = ErrorResult error
wrapResult (Right num) = ValueResult num

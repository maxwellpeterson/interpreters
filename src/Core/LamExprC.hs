module Core.LamExprC where

import qualified Core.Deferred as D
import qualified Core.ExprC as E
import Data.Bifunctor (Bifunctor (second))

data LamExprC a
  = Value a
  | Add (LamExprC a) (LamExprC a)
  | Mul (LamExprC a) (LamExprC a)
  | AppC (LamExprC a) (LamExprC a)
  | IdC E.Identifier
  | LamC E.Identifier (LamExprC a)
  deriving (Show)

data Closure a = Closure E.Identifier (LamExprC a) (D.Environment a)

data InterpError
  = UndefinedFunction E.Name
  | UnboundIdentifier E.Identifier
  | UnexpectedValue
  | UnexpectedClosure
  deriving (Eq, Show)

data InterpResult a
  = ValueResult a
  | ClosureResult (Closure a)
  | ErrorResult InterpError

-- instance Monad InterpResult where
--   return = ValueResult
--   (ValueResult v) >>= f = f v
--   (ClosureResult c) >>= f = ErrorResult UnexpectedClosure -- Violates monad law!?
--   (ErrorResult e) >>= f = ErrorResult e

-- TODO: Write some tests...

interp :: Num a => LamExprC a -> InterpResult a
interp expr = undefined

checkValue :: Num a => LamExprC a -> Either InterpError a
checkValue expr = undefined

checkClosure :: Num a => LamExprC a -> Either InterpError (Closure a)
checkClosure expr = undefined
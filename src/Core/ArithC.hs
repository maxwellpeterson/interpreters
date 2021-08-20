module Core.ArithC
  ( ArithC (..),
    interp,
  )
where

-- Basic arithmetic expressions

data ArithC a = Value a | Add (ArithC a) (ArithC a) | Mul (ArithC a) (ArithC a)

interp :: Num a => ArithC a -> a
interp (Value num) = num
interp (Add left right) = interp left + interp right
interp (Mul left right) = interp left * interp right
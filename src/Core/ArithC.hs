module Core.ArithC
  ( ArithC (..),
    interp,
  )
where

-- Arithmetic Expressions

data ArithC = Value Int | Add ArithC ArithC | Mul ArithC ArithC

interp :: ArithC -> Int
interp (Value num) = num
interp (Add left right) = interp left + interp right
interp (Mul left right) = interp left * interp right
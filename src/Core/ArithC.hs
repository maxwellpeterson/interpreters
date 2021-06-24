module Core.ArithC
  ( ArithC (..),
    interpArithC,
  )
where

-- Arithmetic Expressions

data ArithC = Value Int | Add ArithC ArithC | Mul ArithC ArithC

interpArithC :: ArithC -> Int
interpArithC (Value num) = num
interpArithC (Add left right) = interpArithC left + interpArithC right
interpArithC (Mul left right) = interpArithC left * interpArithC right
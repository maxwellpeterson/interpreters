module Core.BoxExprC
  ( BoxExprC (..),
    Location,
    Environment,
    Store (..),
    Closure (..),
    InterpError (..),
    InterpSuccess (..),
    Threaded (..),
    InterpResult,
    interp
  )
where

import qualified Data.Map as Map

import qualified Core.ExprC as E

-- Dropped multiplication for simplicity
data BoxExprC a
  = Value a
  | Add (BoxExprC a) (BoxExprC a)
  | AppC (BoxExprC a) (BoxExprC a)
  | IdC E.Identifier
  | LamC E.Identifier (BoxExprC a)
  | Box (BoxExprC a)
  | Unbox (BoxExprC a)
  | SetBox (BoxExprC a) (BoxExprC a)
  | Seq (BoxExprC a) (BoxExprC a)
  deriving (Eq, Show)

type Location = Int

type Environment = [(E.Identifier, Location)]

-- Keeps track of next location and current mappings
-- Need to drop previous lazy bindings for mutations to have effect
data Store a = Store Location (Map.Map Location (InterpSuccess a))

data Closure a = Closure E.Identifier (BoxExprC a) Environment deriving (Eq, Show)

data InterpError
  = UnboundIdentifier E.Identifier
  | NonExistentLocation Location
  | TypeMismatch
  deriving (Eq, Show)

data InterpSuccess a
  = ValueResult a
  | ClosureResult (Closure a)
  | BoxResult Location
  deriving (Eq, Show)

data Threaded a = Threaded (InterpSuccess a) (Store a)

type InterpResult a = Either InterpError (InterpSuccess a)

interp :: Num a => BoxExprC a -> InterpResult a
interp expr = do
  -- Discard store before returning successful result
  (Threaded success _) <- interpContext [] (Store 0 Map.empty) expr
  Right success

interpContext :: forall a. Num a =>
  Environment -> Store a -> BoxExprC a -> Either InterpError (Threaded a)
interpContext env store@(Store nextLoc locs) = interpCurrent
  where
    interpCurrent :: BoxExprC a -> Either InterpError (Threaded a)
    interpCurrent (Value num) = Right (Threaded (ValueResult num) store)
    interpCurrent (Add left right) = do
      (Threaded leftOutput leftStore) <- interpContext env store left
      leftNum <- case leftOutput of
        (ValueResult num) -> Right num
        _ -> Left TypeMismatch
      -- Store is threaded from left term to right term
      (Threaded rightOutput rightStore) <- interpContext env leftStore right
      case rightOutput of
        (ValueResult rightNum) -> Right (
          Threaded (ValueResult (leftNum + rightNum)) rightStore
          )
        _ -> Left TypeMismatch
    interpCurrent (AppC func inputExpr) = do
      (Threaded clos funcStore) <- interpCurrent func
      case clos of
        (ClosureResult (Closure identifier body closEnv)) -> do
          (Threaded value (Store nextLoc locs)) <- interpContext env funcStore inputExpr
          interpContext (
            -- Extend closure environment with closure argument
            (identifier, nextLoc) : closEnv
            -- Create location in store for closure argument
            ) (Store (nextLoc + 1) (Map.insert nextLoc value locs)) body
        _ -> Left TypeMismatch
    interpCurrent (IdC identifier) = case lookup identifier env of
      Just loc -> case Map.lookup loc locs of
        Just value -> Right (Threaded value store)
        Nothing -> Left (NonExistentLocation loc)
      Nothing -> Left (UnboundIdentifier identifier)
    interpCurrent (LamC identifier body) = Right (
      Threaded (ClosureResult (Closure identifier body env)) store
      )
    interpCurrent (Box expr) = do
      (Threaded value (Store nextLoc locs)) <- interpCurrent expr
      -- Create location in store for boxed expression, and store location in box
      Right (Threaded (BoxResult nextLoc)
             (Store (nextLoc + 1) (Map.insert nextLoc value locs)))
    interpCurrent (Unbox expr) = do
      (Threaded box nextStore@(Store _ locs)) <- interpCurrent expr
      case box of
        (BoxResult loc) -> case Map.lookup loc locs of
          Just value -> Right (Threaded value nextStore)
          Nothing -> Left (NonExistentLocation loc)
        _ -> Left TypeMismatch
    interpCurrent (SetBox boxExpr expr) = do
      (Threaded box boxStore) <- interpCurrent boxExpr
      case box of
        (BoxResult loc) -> do
          (Threaded value (Store nextLoc locs)) <- interpContext env boxStore expr
          Right (
            -- Override value at location stored by box
            Threaded (BoxResult loc) (Store nextLoc (Map.insert loc value locs))
            )
        _ -> Left TypeMismatch
    interpCurrent (Seq firstExpr secondExpr) = do
      -- Thread store between two sequential expressions
      (Threaded _ firstStore) <- interpCurrent firstExpr
      interpContext env firstStore secondExpr

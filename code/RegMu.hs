{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RegMu
(
  Reg (..),
  PVal (..),
  Var,
  flatten
)
where

import Prelude hiding (sum)
import Data.Char (chr)

-- variables
newtype Var = Var_ Integer deriving (Eq, Ord, Show, Enum, Num)

data Reg a
  = O                         -- | 0
  | E                         -- | 1
  | Lit a                     -- | literal
  | Reg a :+: Reg a           -- | +
  | Reg a :*: Reg a           -- | x
  | Var Var                   -- | variable
  | Mu Var (Reg a)            -- | mu-recursion
  deriving (Eq, Ord)

data PVal a
  = Unit                      -- | ()
  | Char a                    -- | constant
  | Inl (PVal a)              -- | inl
  | Inr (PVal a)              -- | inr
  | Pair (PVal a) (PVal a)    -- | (,)
  | In ([PVal a])
  | Fold (PVal a)             -- | fold
  deriving (Eq, Ord)


instance Show a => Show (Reg a) where
  show O         = error "Can't show 'O' (Empty set)"
  show E         = ""
  show (Lit c)   = show c
  show (e :+: f) = 
      case f of 
        (_ :*: _) -> show e ++ "|(" ++ show f ++ ")"
        _ -> show e ++ "|" ++ show f
  show (e :*: f) = 
      case f of
        (_ :+: _) ->       show e ++ "(" ++ show f ++ ")"
        _ ->       show e ++ show f

  show (Var t)   = show t
  show (Mu t r)  = "(\\ " ++ show t ++ " . " ++ show r ++ ")"

instance Show a => Show (PVal a) where
  show Unit         = "()"
  show (Char c)     = show c
  show (Inl v)      = "inl " ++ show v
  show (Inr w)      = "inr " ++ show w
  show (v `Pair` w) = "(" ++ show v ++ "," ++ show w ++ ")"
  show (Fold v)     = "fold " ++ show v

flatten :: PVal a -> [a]
flatten Unit         = []
flatten (Char x)     = [x]
flatten (Inl v)      = flatten v
flatten (Inr w)      = flatten w
flatten (v `Pair` w) = flatten v ++ flatten w
flatten (Fold v)     = flatten v

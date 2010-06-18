{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RegSum
(
  Reg (..),
  Var,
  Label,
  Choice (..),
  Path
)
where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map, (!))
import qualified Data.Map as Map

newtype Var = Var_ Integer deriving (Eq, Ord, Show, Enum, Num)

newtype Label = Label Integer deriving (Eq, Ord, Show, Enum, Num)

data Choice = L | R deriving (Ord, Eq, Show)

type Path = [Choice]

data Reg a
  = O
  | E
  | Lit a
  | Sigma Label (Map Path (Reg a))
  | Reg a :*: Reg a
  | Var Var
  | Mu Var (Reg a)
  | Star (Reg a)
    deriving (Show)

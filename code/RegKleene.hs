{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RegKleene
(
  Reg (..),
  PVal (..),
  flatten,
  prod,
  sum,
  alpha,
  loweralpha,
  upperalpha,
  num,
  alphanum,
  dot,
  string,
  cclass,
  pcdata
)
where

import Prelude hiding (sum)
import Data.Char (chr)

data Reg a
  = O                         -- | 0
  | E                         -- | 1
  | Lit a                     -- | literal
  | Reg a :+: Reg a           -- | +
  | Reg a :*: Reg a           -- | x
  | Star (Reg a)              -- | kleene-star
  deriving (Eq, Ord)

data PVal a
  = Unit                      -- | ()
  | Char a                    -- | constant
  | Inl (PVal a)              -- | inl
  | Inr (PVal a)              -- | inr
  | Pair (PVal a) (PVal a)    -- | (,)
  | Fold ([PVal a])
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
  show (Star r)  = 
      case r of 
        (_ :*: _) -> "(" ++ show r ++ ")*"
        (_ :+: _) -> "(" ++ show r ++ ")*"
        E -> ""
        _ -> show r ++ "*"

{-
instance Show a => Show (Regex a) where
  show O         = "o"
  show E         = "e"
  show (Lit c)   = show c
  show (e :+: f) = "(" ++ show e ++ " + " ++ show f ++ ")"
  show (e :*: f) = "(" ++ show e ++ " x " ++ show f ++ ")"
  show (Var t)   = show t
  show (Mu t r)  = "(\\ " ++ show t ++ " . " ++ show r ++ ")"
  show (Star r)  = show r ++ "*"
-}

instance Show a => Show (PVal a) where
  show Unit         = "()"
  show (Char c)     = show c
  show (Inl v)      = "inl " ++ show v
  show (Inr w)      = "inr " ++ show w
  show (v `Pair` w) = "(" ++ show v ++ "," ++ show w ++ ")"
  show (Fold xs)    = "in " ++ show xs

flatten :: PVal a -> [a]
flatten Unit         = []
flatten (Char x)     = [x]
flatten (Inl v)      = flatten v
flatten (Inr w)      = flatten w
flatten (v `Pair` w) = flatten v ++ flatten w
flatten (Fold vs)    = concat $ map flatten vs

sum :: [Reg a] -> Reg a
sum []     = E
sum (c:cs) = foldl (:+:) c cs

prod :: [Reg a] -> Reg a
prod []     = O
prod (c:cs) = foldl (:*:) c cs

alpha :: Reg Char
alpha = loweralpha :+: upperalpha

loweralpha :: Reg Char
loweralpha = cclass ['a' .. 'z']

upperalpha :: Reg Char
upperalpha = cclass ['A' .. 'Z']

num :: Reg Char
num = cclass $ ['0' .. '9']

alphanum :: Reg Char
alphanum = alpha :+: num

dot :: Reg Char
dot = cclass $ fmap chr $ [1 .. 255]

string :: [a] -> Reg a
string = prod . fmap Lit

cclass :: [a] -> Reg a
cclass = sum . fmap Lit

pcdata :: Reg Char
pcdata = cclass $ fmap chr $ [32 .. 126] -- most printable chars.

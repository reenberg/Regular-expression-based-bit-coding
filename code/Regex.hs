module Regex
(
  Regex (..),
  STree (..),
  Var,
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

-- variables
type Var = Int

data Regex a
  = O                         -- | 0
  | E                         -- | 1
  | Lit a                     -- | literal
  | Regex a :+: Regex a       -- | +
  | Regex a :*: Regex a       -- | x
  | Var Var                   -- | variable
  | Mu Var (Regex a)          -- | mu-recursion
  | Star (Regex a)            -- | kleene-star
  deriving (Eq, Ord)

data STree a
  = Unit                      -- | ()
  | Char a                    -- | constant
  | Inl (STree a)             -- | inl
  | Inr (STree a)             -- | inr
  | Pair (STree a) (STree a)  -- | (,)
  | In ([STree a])
  | Fold (STree a)            -- | fold
  deriving (Eq, Ord)

instance Show a => Show (Regex a) where
  show O         = error "Can't show 'O' (Empty set)"
  show E         = ""
  show (Lit c)   = show c
  show (e :+: f) = show e ++ "|" ++ show f
  show (e :*: f) = show e ++ show f
  show (Var t)   = show t
  show (Mu t r)  = "(\\ " ++ show t ++ " . " ++ show r ++ ")"
  show (Star r)  = 
      case r of 
        (_ :*: _) -> "(" ++ show r ++ ")*"
        (_ :+: _) -> "(" ++ show r ++ ")*"
        E -> ""
        _ -> show r ++ "*"
{-
  show O         = "o"
  show E         = "e"
  show (Lit c)   = show c
  show (e :+: f) = "(" ++ show e ++ " + " ++ show f ++ ")"
  show (e :*: f) = "(" ++ show e ++ " x " ++ show f ++ ")"
  show (Var t)   = show t
  show (Mu t r)  = "(\\ " ++ show t ++ " . " ++ show r ++ ")"
  show (Star r)  = show r ++ "*"
-}

instance Show a => Show (STree a) where
  show Unit         = "()"
  show (Char c)     = show c
  show (Inl v)      = "inl " ++ show v
  show (Inr w)      = "inr " ++ show w
  show (v `Pair` w) = "(" ++ show v ++ "," ++ show w ++ ")"
  show (Fold v)     = "fold " ++ show v
  show (In xs)      = "in " ++ show xs

flatten :: STree a -> [a]
flatten Unit         = []
flatten (Char x)     = [x]
flatten (Inl v)      = flatten v
flatten (Inr w)      = flatten w
flatten (v `Pair` w) = flatten v ++ flatten w
flatten (Fold v)     = flatten v
flatten (In vs)      = concat $ map flatten vs

sum :: [Regex a] -> Regex a
sum []     = E
sum (c:cs) = foldl (:+:) c cs

prod :: [Regex a] -> Regex a
prod []     = O
prod (c:cs) = foldl (:*:) c cs

alpha :: Regex Char
alpha = loweralpha :+: upperalpha

loweralpha :: Regex Char
loweralpha = cclass ['a' .. 'z']

upperalpha :: Regex Char
upperalpha = cclass ['A' .. 'Z']

num :: Regex Char
num = cclass $ ['0' .. '9']

alphanum :: Regex Char
alphanum = alpha :+: num

dot :: Regex Char
dot = cclass $ fmap chr $ [1 .. 255]

string :: [a] -> Regex a
string = prod . fmap Lit

cclass :: [a] -> Regex a
cclass = sum . fmap Lit

pcdata :: Regex Char
pcdata = cclass $ fmap chr $ [32 .. 126] -- most printable chars.

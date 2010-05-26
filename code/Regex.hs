module Regex
(
    Regex (..),
    STree (..),
    prod,
    sum,
    alpha,
    num,
    alphanum,
    dot,
    string
)
where

import Prelude hiding (sum)
import Data.Char (chr)

data Regex a
    = O                         -- | 0
    | E                         -- | 1
    | Lit a                     -- | literal
    | Regex a :+: Regex a       -- | +
    | Regex a :*: Regex a       -- | x
    | S (Regex a)               -- | *
    deriving (Eq, Ord)

data STree a
    = Unit                      -- | ()
    | Char a                    -- | constant
    | Inl (STree a)             -- | inl
    | Inr (STree a)             -- | inr
    | Pair (STree a) (STree a)  -- | (,)
    | Fold (STree a)            -- | fold
    deriving (Eq, Ord)

instance Show a => Show (Regex a) where
    show O         = "0"
    show E         = "1"
    show (Lit c)   = show c
    show (e :+: f) = "(" ++ show e ++ " + " ++ show f ++ ")"
    show (e :*: f) = "(" ++ show e ++ " x " ++ show f ++ ")"
    show (S e)     = show e ++ "*"

instance Show a => Show (STree a) where
    show Unit         = "()"
    show (Char c)     = show c
    show (Inl v)      = "inl " ++ show v
    show (Inr w)      = "inr " ++ show w
    show (v `Pair` w) = "(" ++ show v ++ "," ++ show w ++ ")"
    show (Fold v)     = "fold " ++ show v

sum :: [a] -> Regex a
sum = foldl (:+:) E . fmap Lit

prod:: [a] -> Regex a
prod = foldl (:*:) E . fmap Lit

alpha :: Regex Char
alpha = sum $ ['a' .. 'z'] ++ ['A' .. 'Z']

num :: Regex Char
num = sum $ ['0' .. '9']

alphanum :: Regex Char
alphanum = alpha :+: num

dot :: Regex Char
dot = sum $ fmap chr $ [0 .. 127]

string :: String -> Regex Char
string = prod

module Regex
(
    Regex (..),
    STree (..),
    prod,
    sum,
    alpha,
    loweralpha,
    upperalpha,
    num,
    alphanum,
    dot,
    string,
    balance
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

balance :: Regex a -> Regex a
balance r =
    case r of
      O -> O
      E -> E
      Lit a -> Lit a
      r1 :*: r2 -> balance r1 :*: balance r2
      S r -> S $ balance r
      _ :+: _ -> tree . map balance $ gather r
          where
            gather (r1 :+: r2) = gather r1 ++ gather r2
            gather r = [r]
            tree [] = O
            tree [r] = r
            tree rs = tree rs1 :+: tree rs2
                where
                  (rs1, rs2) = splitAt (length rs `div` 2) rs

sum :: [a] -> Regex a
sum = foldl (:+:) E . fmap Lit

prod :: [a] -> Regex a
prod = foldl (:*:) E . fmap Lit

alpha :: Regex Char
alpha = loweralpha :+: upperalpha

loweralpha :: Regex Char
loweralpha = sum ['a' .. 'z']

upperalpha :: Regex Char
upperalpha = sum ['A' .. 'Z']

num :: Regex Char
num = sum $ ['0' .. '9']

alphanum :: Regex Char
alphanum = alpha :+: num

dot :: Regex Char
dot = sum $ fmap chr $ [0 .. 127]

string :: String -> Regex Char
string = prod

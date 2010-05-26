module Regex
(
    Regex (..),
    STree (..)
)
where

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

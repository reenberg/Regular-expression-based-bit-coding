module Coding
(
    Bit (..),
    code,
    decode
)
where

import Consume (Consume, pop, evalConsume)
import Control.Monad (liftM, liftM2)
import Control.Monad.Writer (Writer, tell, execWriter)
import Regex (Regex (..), STree (..))

data Bit = B0 | B1 deriving (Eq, Ord)

instance Show Bit where
  show B0 = "0"
  show B1 = "1"

code1 :: Eq a => STree a -> Regex a -> Writer [Bit] ()
code1 Unit         E                = return ()
code1 (Char x)     (Lit y) | x == y = return ()
code1 (Inl v)      (e :+: _)        = tell [B0] >> code1 v e
code1 (Inr w)      (_ :+: f)        = tell [B1] >> code1 w f
code1 (v `Pair` w) (e :*: f)        = code1 v e >> code1 w f
code1 (Fold v)     (S e)            = code1 v (E :+: (e :*: S e))

code :: Eq a => Regex a -> STree a -> [Bit]
code e v = execWriter (code1 v e)

decode1 :: Eq a => Regex a -> Consume Bit (STree a)
decode1 E         = return Unit
decode1 (Lit a)   = return (Char a)
decode1 (e :+: f) = pop >>= \b -> case b of B0 -> liftM Inl (decode1 e)
                                            B1 -> liftM Inr (decode1 f)
decode1 (e :*: f) = liftM2 Pair (decode1 e) (decode1 f)
decode1 (S e)     = liftM Fold (decode1 (E :+: (e :*: S e)))

decode :: Eq a => Regex a -> [Bit] -> STree a
decode e bs = evalConsume bs (decode1 e)

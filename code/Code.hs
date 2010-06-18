module Code
(
    Bit (..),
    code,
    decode
)
where

import Data.Map (Map)
import qualified Data.Map as Map
import Consume (Consume, pop, evalConsume)
import Control.Monad (liftM, liftM2)
import Control.Monad.Writer (Writer, tell, execWriter)
import RegMu (Reg (..), PVal (..), Var)

data Bit = B0 | B1 deriving (Eq, Ord)

type Env a = Map Var (Reg a)

instance Show Bit where
  show B0 = "0"
  show B1 = "1"

code1 :: (Show a, Eq a) => PVal a -> Reg a -> Env a -> Writer [Bit] ()
code1 Unit         E           _   = return ()
code1 (Char x)     (Lit y)     _   | x == y = return ()
code1 (Inl v)      (r1 :+: _)  env = tell [B0] >> code1 v r1 env
code1 (Inr w)      (_  :+: r2) env = tell [B1] >> code1 w r2 env
code1 (v `Pair` w) (r1 :*: r2) env = code1 v r1 env >> code1 w r2 env
code1 v            (Var t)     env = case Map.lookup t env of Just r -> code1 v r env
code1 (Fold v)     (Mu t r)    env = code1 v r (Map.insert t (Mu t r) env)
code1 r            v           _   = error $ "code1: " ++ show r ++ "\n" ++ show v

code :: (Show a, Eq a) => Reg a -> PVal a -> [Bit]
code e v = execWriter (code1 v e Map.empty)

decode1 :: Eq a => Reg a -> Env a -> Consume Bit (PVal a)
decode1 E           _   = return Unit
decode1 (Lit a)     _   = return (Char a)
decode1 (r1 :+: r2) env = pop >>= \b ->
  case b of
    B0 -> liftM Inl (decode1 r1 env)
    B1 -> liftM Inr (decode1 r2 env)
decode1 (r1 :*: r2) env = liftM2 Pair (decode1 r1 env) (decode1 r2 env)
decode1 (Var t)     env = case Map.lookup t env of Just r -> decode1 r env
decode1 (Mu t r)    env = liftM Fold (decode1 r (Map.insert t (Mu t r) env))

decode :: Eq a => Reg a -> [Bit] -> PVal a
decode e bs = evalConsume bs (decode1 e Map.empty)

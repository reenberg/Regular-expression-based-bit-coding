module Parse
(
    parse
)
where

import System.IO.Unsafe
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (MonadPlus, liftM, liftM2, mzero, mplus, msum)
import RegMu (Reg (..), PVal (..), Var)

type Env a = Map Var (Reg a, [a])

type Parse a = [(PVal a, [a])]

parse1 :: (Eq a) => Reg a -> [a] -> Env a -> Parse a
parse1 O xs _ = []
parse1 E xs _ = [(Unit, xs)]
parse1 (Lit y) [] _ = []
parse1 (Lit y) (x:xs) _ | x == y    = [(Char x, xs)]
                        | otherwise = -- showit (y, x) `seq`
                                      []
parse1 (r1 :*: r2) xs env =
  [ (Pair p1 p2, zs) | (p1, ys) <- parse1 r1 xs env, (p2, zs) <- parse1 r2 ys env ]
parse1 (r1 :+: r2) xs env =
  [ (Inl p1, ys) | (p1, ys) <- parse1 r1 xs env ] ++
  [ (Inr p2, zs) | (p2, zs) <- parse1 r2 xs env ]
parse1 (Var t) xs env =
  case Map.lookup t env of
    Just (r, ys) -> if length ys > length xs then
                      [ (p, zs) | (p, zs) <- parse1 r xs env ]
                    else
                      []
parse1 (Mu t r) xs env = [ (Fold p, xs) | (p, xs) <- parse1 r xs env' ]
  where
    env' = Map.insert t (Mu t r, xs) env

parse :: Eq a => Reg a -> [a] -> Maybe (PVal a)
parse r cs = case filter (null . snd) (parse1 r cs Map.empty) of
  (v, _) : _ -> Just v
  _          -> Nothing

{-
parse1 :: (MonadPlus m, Eq a) => Reg a -> [a] -> Env a -> m (STree a)
parse1 O           _   _   = mzero
parse1 E           []  _   = return Unit
--parse1 E           _   _   = mzero
parse1 (Lit x)     [y] _   | x == y = return (Char x)
--parse1 (Lit _)     _   _   = mzero
parse1 (r1 :+: r2) cs  env = liftM Inl (parse1 r1 cs env) `mplus` liftM Inr (parse1 r2 cs env)
parse1 (r1 :*: r2) cs  env = msum (parsePair `fmap` splits)
                               where
                                 splits = [ splitAt n cs | n <- [0..length cs] ]
                                 parsePair (cs1,cs2) = liftM2 Pair
                                   (parse1 r1 cs1 env) (parse1 r2 cs2 env)
--parse1 (Var t)     cs  env = case Map.lookup t env of Just r -> parse1 r cs env
--parse1 (Mu t r)    cs  env = liftM Fold (parse1 r cs (Map.insert t (Mu t r) env))
parse1 (Star e)    []  _   = return ((Fold . Inl) Unit)
parse1 (Star r)    cs  env = liftM (Fold . Inr) (msum (parsePair `fmap` splits))
                               where
                                 splits = [ splitAt n cs | n <- [1..length cs] ]
                                 parsePair (cs1,cs2) = liftM2 Pair
                                   (parse1 r cs1 env) (parse1 (Star r) cs2 env)
parse1 _           _   _   = mzero

parse :: Eq a => Regex a -> [a] -> Maybe (STree a)
parse r cs = parse1 r cs Map.empty
-}

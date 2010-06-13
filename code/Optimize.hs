module Optimize
(
  specialize,
  optimize,
  balance
)
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (head, partition)
import Data.Maybe (fromJust)
import Regex (Regex (..), STree (..), Var)
import Control.Monad.State (State, get, put, evalState)
import Control.Monad (liftM, liftM2)

optimize :: Regex a -> Regex a
optimize = balance . normalize

normalize1 :: Regex a -> State Int (Regex a)
normalize1 r =
  case r of
    r1 :*: (r2 :+: r3) -> liftM2 (:+:) (normalize1 $ r1 :*: r2) (normalize1 $ r1 :*: r3)
    (r1 :+: r2) :*: r3 -> liftM2 (:+:) (normalize1 $ r1 :*: r3) (normalize1 $ r2 :*: r3)
    r1 :+: r2 -> liftM2 (:+:) (normalize1 r1) (normalize1 r2)
    r1 :*: r2 -> liftM2 (:*:) (normalize1 r1) (normalize1 r2)
    Mu t r -> liftM (Mu t) (normalize1 r)
    Star r -> do t <- get
                 put (t + 1)
                 normalize1 (Mu t ((r :*: Var t) :+: E))
    r -> return r

normalize :: Regex a -> Regex a
normalize r = evalState (normalize1 r) 0

data Choice = L | R deriving (Eq)
type Path = [Choice]
specialize :: Regex a -> STree a -> Regex a
specialize r v = loop r (paths r v)
    where
      loop :: Regex a -> [Path] -> Regex a
      loop r ps =
          case r of
            _ :+: _ -> hoffman (length ps) $
                       fmap (\(r, ps) -> (loop r ps, length ps)) $
                       choices r ps
            r1 :*: r2 -> loop r1 ps :*: loop r2 ps
            Star r' -> loop r' ps
            Mu t r' -> Mu t $ loop r' ps
            _ -> r

      choices :: Regex a -> [Path] -> [(Regex a, [Path])]
      choices r ps =
          case r of
            r1 :+: r2 ->
                 choices r1 lps ++ choices r2 rps
                     where
                       (lps, rps) = partition ((== L) . head) ps
            _ -> [(r, ps)]

      hoffman :: Int -> [(Regex a, Int)] -> Regex a
      hoffman _ [] = E
      hoffman _ ((r, _) : rs) = foldl (:+:) r $ fmap fst rs

      paths :: Regex a -> STree a -> [Path]
      paths r v = loop Map.empty [] r v
          where
            loop e p r v =
                case (r, v) of
                  (r1 :*: r2, v1 `Pair` v2) ->
                      (loop e p r1 v1) ++ (loop e p r2 v2)
                  (Var t, _) ->
                      p : loop e p' r' v
                          where
                            (r', p') = fromJust $ Map.lookup t e
                  (Mu t r', Fold v') ->
                      loop (Map.insert t (r', p) e) p r' v'
                  (Star r', In vs) ->
                      concat $ fmap (loop e p r') vs
                  (r' :+: _, Inl v') ->
                      loop e (L : p) r' v'
                  (_ :+: r', Inr v') ->
                      loop e (R : p) r' v'
                  _ ->
                      [p]

balance :: Regex a -> Regex a
balance r =
    case r of
      O -> O
      E -> E
      Lit a -> Lit a
      r1 :*: r2 -> balance r1 :*: balance r2
      Var t -> Var t
      Mu t r -> Mu t (balance r)
      Star r -> Star $ balance r
      _ :+: _ -> tree . map balance $ gather r
          where
            gather (r1 :+: r2) = gather r1 ++ gather r2
            gather r = [r]
            tree [] = O
            tree [r] = r
            tree rs = tree rs1 :+: tree rs2
                where
                  (rs1, rs2) = splitAt (length rs `div` 2) rs

r1 = Mu 0 (E :+: (r2 :*: Var 0))
r2 = Lit 'a' :+: Lit 'b' :+: Lit 'c'
r3 = Star r2
r4 = Star r3

main :: IO ()
main =
  do print $ optimize r4

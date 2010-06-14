module Optimize
(
  specialize,
  optimize,
  normalize,
  balance
)
where

import System.IO.Unsafe
import Data.Either (lefts, rights)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Data.List as List
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

data Choice = L | R
type Path = [Choice]

showit x = (unsafePerformIO $ print x) `seq` x

specialize :: (Ord a, Show a) => Regex a -> STree a -> Regex a
specialize r v = loop r (paths r v)
    where
      loop :: (Ord a, Show a) => Regex a -> [Path] -> Regex a
      loop r ps =
          case r of
            _ :+: _ -> hoffman $
                       Set.fromList $
                       fmap (\(r, ps) -> (length ps, loop r ps)) $
                       -- showit $
                       choices r ps
            r1 :*: r2 -> loop r1 psf :*: loop r2 pss
                where
                  (psf, pss) = unzip $
                               fmap
                               (\ps ->
                                    case ps of
                                      P l r -> (l, r)
                               ) ps
            Star r' -> Star $ loop r' ps
            Mu t r' -> Mu t $ loop r' ps
            _ -> r

      choices :: Regex a -> [Path] -> [(Regex a, [Path])]
      choices r ps =
          case r of
            r1 :+: r2 ->
                 choices r1 (lefts ps') ++ choices r2 (rights ps')
                     where
                       ps' = fmap (\ps -> case ps of
                                            L p -> Left p
                                            R p -> Right p) ps
            _ -> [(r, ps)]

      hoffman :: (Ord a, Show a) => Set (Int, Regex a) -> Regex a
      hoffman s = showit s `seq`
          let ((n1, r1), s') = fromJust $ Set.minView s
          in case Set.minView s' of
               Just ((n2, r2), s'') ->
                   hoffman (Set.insert (n1 + n2, r1 :+: r2) s'')
               Nothing -> r1

      paths :: Regex a -> STree a -> [Path]
      paths r v = loop Map.empty [] r v
          where
            loop e p r v =
                case (r, v) of
                  (r1 :*: r2, v1 `Pair` v2) ->
                      [l ++ r | l <- (loop e p r1 v1), r <- (loop e l r2 v2)]
                  (Var t, _) ->
                      loop e p' r' v
                          where
                            (r', p') = e ! t
                  (Mu t r', Fold v') ->
                      loop (Map.insert t (r, p) e) p r' v'
                  (Star r', In vs) ->
                      concat $ fmap (loop e p r') vs
                  (r' :+: _, Inl v') ->
                      loop e (L : p) r' v'
                  (_ :+: r', Inr v') ->
                      loop e (R : p) r' v'
                  _ -> [p]

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

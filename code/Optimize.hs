module Optimize
(
  specialize,
  optimize,
  normalize,
  balance
)
where

import Control.Arrow (first, second)
import System.IO.Unsafe
import Data.Either (lefts, rights)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map) -- , (!))
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

data Choice = L | R deriving (Ord, Eq, Show)
type Path = [Choice]

data Regex' a
    = O'
    | E'
    | Lit' a
    | Plus' Int (Map Path (Regex' a))
    | Regex' a :**: Regex' a
    | Var' Var
    | Mu' Var (Regex' a)
    | Star' (Regex' a)

showit x = (unsafePerformIO $ print x) `seq` x

m ! k = case Map.lookup k m of
          Just a -> a
          Nothing -> error $ show k

specialize :: (Ord a, Show a) => Regex a -> STree a -> Regex a
specialize r v = arrange r' (count r' v)
    where
      r' = convert r

      convert :: (Ord a, Show a) => Regex a -> Regex' a
      convert r = evalState (loop r) 0
          where
            loop r =
                case r of
                  O -> return O'
                  E -> return E'
                  Lit a -> return $ Lit' a
                  r1 :*: r2 -> liftM2 (:**:) (loop r1) (loop r2)
                  Var t -> return $ Var' t
                  Mu t r' -> liftM (Mu' t) (loop r')
                  Star r' -> liftM Star' (loop r')
                  _ :+: _ ->
                      do n <- get
                         put (n + 1)
                         m <- gather r
                         return $ Plus' n m
                             where
                               gather (r1 :+: r2) =
                                   do r1' <- gather r1
                                      r2' <- gather r2
                                      return $ Map.union (Map.mapKeys (L :) r1') (Map.mapKeys (R :) r2')
                               gather r = liftM (Map.singleton []) $ loop r

      count :: (Ord a, Show a) => Regex' a -> STree a -> Map Var (Map Path Int)
      count r v = loop Map.empty r v
          where
            loop e r v =
                case (r, v) of
                  (r1 :**: r2, v1 `Pair` v2) -> Map.union (loop e r1 v1) (loop e r2 v2)
                  (Var' t, _) -> loop e (e ! t) v
                  (Mu' t r', Fold v') -> loop (Map.insert t r e) r' v'
                  (Star' r', In vs) -> Map.unions $ fmap (loop e r') vs
                  (Plus' t rs, _) ->
                      adjustWithDefault Map.empty t (adjustWithDefault 0 p (+ 1)) $
                      loop e (rs ! p) v'
                      where
                        (p, v') = follow v
                        follow v =
                            case v of
                              Inl v' -> first ((:) L) $ follow v'
                              Inr v' -> first ((:) L) $ follow v'
                              _ -> ([], v)
                        adjustWithDefault d k f m =
                            case Map.lookup k m of
                              Just a -> Map.insert k (f a) m
                              Nothing -> Map.insert k (f d) m
                  _ -> Map.empty

      arrange :: (Ord a, Show a) => Regex' a -> Map Var (Map Path Int) -> Regex a
      arrange r m = loop (fmap (Set.fromList . fmap swap . Map.toList) m) r
          where
            swap (a, b) = (b, a)
            loop :: (Ord a, Show a) => Map Var (Set (Int, Path)) -> Regex' a -> Regex a
            loop m r =
                case r of
                  O' -> O
                  E' -> E
                  Lit' a -> Lit a
                  r1 :**: r2 -> loop m r1 :*: loop m r2
                  Var' t -> Var t
                  Mu' t r' -> Mu t $ loop m r'
                  Star' r' -> Star $ loop m r'
                  Plus' t rs -> hoffman $ Set.map (second (rs' !)) $ m ! t
                      where
                        rs' = fmap (loop m) rs

      hoffman :: (Ord a, Show a) => Set (Int, Regex a) -> Regex a
      hoffman s = -- showit s `seq`
          let ((n1, r1), s') = fromJust $ Set.minView s
          in case Set.minView s' of
               Just ((n2, r2), s'') ->
                   hoffman (Set.insert (n1 + n2, r1 :+: r2) s'')
               Nothing -> r1

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

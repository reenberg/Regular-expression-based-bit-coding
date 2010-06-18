{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe (fromJust)
import Regex (Regex (..), STree (..), Var)
import Control.Monad.State (State, get, put, evalState)
import Control.Monad (liftM, liftM2)

data Choice = L | R deriving (Ord, Eq, Show)
type Path = [Choice]

newtype Label = Label Integer deriving (Eq, Ord, Show, Enum, Num)

data Regex' a
  = O'
  | E'
  | Lit' a
  | Sigma' Label (Map Path (Regex' a))
  | Regex' a :**: Regex' a
  | Var' Var
  | Mu' Var (Regex' a)
  | Star' (Regex' a)
    deriving (Show)

showit x = (unsafePerformIO $ print x) `seq` x

inc :: Num a => State a a
inc = do { n <- get ; put (n+1) ; return n }

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

adjustWithDefault :: (Ord k) => a -> k -> (a -> a) -> Map k a -> Map k a
adjustWithDefault d k f m =
  case Map.lookup k m of
    Just a  -> Map.insert k (f a) m
    Nothing -> Map.insert k (f d) m

optimize :: Regex a -> Regex a
optimize = balance . normalize

normalize1 :: Regex a -> State Var (Regex a)
normalize1 r =
  case r of
    r1 :*: (r2 :+: r3) -> liftM2 (:+:) (normalize1 $ r1 :*: r2) (normalize1 $ r1 :*: r3)
    (r1 :+: r2) :*: r3 -> liftM2 (:+:) (normalize1 $ r1 :*: r3) (normalize1 $ r2 :*: r3)
    -- r1 :+: Mu t r2 -> normalize1 $ Mu t (r1 :+: r2)
    -- Mu t r1 :+: r2 -> normalize1 $ Mu t (r1 :+: r2)
    r1 :+: r2 -> liftM2 (:+:) (normalize1 r1) (normalize1 r2)
    r1 :*: r2 -> liftM2 (:*:) (normalize1 r1) (normalize1 r2)
    Mu t r -> liftM (Mu t) (normalize1 r)
    Star r -> do t <- inc
                 normalize1 (Mu t ((r :*: Var t) :+: E))
    r -> return r

normalize :: Regex a -> Regex a
normalize r = evalState (normalize1 r) 0

specialize :: (Ord a, Show a) => Regex a -> STree a -> Regex a
specialize r = arrange r' . count r'
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
          do n <- inc
             m <- gather r
             return $ Sigma' n m
               where
                 gather (r1 :+: r2) =
                   do r1' <- gather r1
                      r2' <- gather r2
                      return $ Map.union (Map.mapKeys (L :) r1') (Map.mapKeys (R :) r2')
                 gather r = liftM (Map.singleton []) $ loop r

count :: (Ord a, Show a) => Regex' a -> STree a -> Map Label (Map Path Int)
count r v = loop Map.empty r v
  where
    loop e r v =
      case (r, v) of
        (r1 :**: r2, v1 `Pair` v2) -> merge (loop e r1 v1) (loop e r2 v2)
            where
              merge = Map.unionWith $ Map.unionWith (+)
        (Var' t, _) -> loop e (e ! t) v
        (Mu' t r', Fold v') -> loop (Map.insert t r e) r' v'
        -- Star isn't working!!!
        -- (Star' r', In vs) -> Map.unions $ fmap (loop e r') vs
        (Sigma' t rs, _) ->
          adjustWithDefault Map.empty t (adjustWithDefault 0 p (+ 1)) $
            loop e (rs ! p) v'
          where
            (p, v') = follow v
            follow v =
              case v of
                Inl v' -> first (L :) $ follow v'
                Inr v' -> first (R :) $ follow v'
                _ -> ([], v)
        _ -> Map.empty

arrange :: (Ord a, Show a) => Regex' a -> Map Label (Map Path Int) -> Regex a
arrange r m = loop (fmap (Set.fromList . fmap swap . Map.toList) m) r
  where
    loop :: (Ord a, Show a) => Map Label (Set (Int, Path)) -> Regex' a -> Regex a
    loop m r =
      case r of
        O' -> O
        E' -> E
        Lit' a -> Lit a
        r1 :**: r2 -> loop m r1 :*: loop m r2
        Var' t -> Var t
        Mu' t r' -> Mu t $ loop m r'
        Star' r' -> Star $ loop m r'
        Sigma' t rs -> hoffman $ Set.map (second (rs' !)) $ m ! t
          where
            rs' = fmap (loop m) rs

hoffman :: (Ord a, Show a) => Set (Int, Regex a) -> Regex a
hoffman s =
  case Set.minView s' of
    Just ((n2, r2), s'') -> hoffman (Set.insert (n1 + n2, r1 :+: r2) s'')
    Nothing -> r1
  where
    ((n1, r1), s') = fromJust $ Set.minView s

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

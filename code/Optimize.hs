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
import Control.Monad.State (State, get, put, evalState)
import Control.Monad (liftM, liftM2)

import qualified RegKleene as K
import qualified RegMu as M
import qualified RegSum as S
import RegMu (PVal(..))
import RegSum (Choice (..), Path, Label)

inc :: Num a => State a a
inc = do { n <- get ; put (n+1) ; return n }

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

adjustWithDefault :: (Ord k) => a -> k -> (a -> a) -> Map k a -> Map k a
adjustWithDefault d k f m =
  case Map.lookup k m of
    Just a  -> Map.insert k (f a) m
    Nothing -> Map.insert k (f d) m

optimize :: K.Reg a -> M.Reg a
optimize = balance . normalize

normalize1 :: K.Reg a -> State M.Var (M.Reg a)
normalize1 r =
  case r of
    K.O -> return M.O
    K.E -> return M.E
    r1 K.:*: (r2 K.:+: r3) -> liftM2 (M.:+:) (normalize1 $ r1 K.:*: r2) (normalize1 $ r1 K.:*: r3)
    (r1 K.:+: r2) K.:*: r3 -> liftM2 (M.:+:) (normalize1 $ r1 K.:*: r3) (normalize1 $ r2 K.:*: r3)
    r1 K.:+: r2 -> liftM2 (M.:+:) (normalize1 r1) (normalize1 r2)
    r1 K.:*: r2 -> liftM2 (M.:*:) (normalize1 r1) (normalize1 r2)
    K.Star r -> do t <- inc
                   r' <- normalize1 r
                   return $ M.Mu t ((r' M.:*: M.Var t) M.:+: M.E)

normalize :: K.Reg a -> M.Reg a
normalize r = evalState (normalize1 r) 0

specialize :: (Ord a, Show a) => M.Reg a -> M.PVal a -> M.Reg a
specialize r = arrange r' . count r'
  where
    r' = convert r

convert :: (Ord a, Show a) => M.Reg a -> S.Reg a
convert r = evalState (loop r) 0
  where
    loop r =
      case r of
        M.O -> return S.O
        M.E -> return S.E
        M.Lit a -> return $ S.Lit a
        r1 M.:*: r2 -> liftM2 (S.:*:) (loop r1) (loop r2)
        M.Var t -> return $ S.Var (toEnum $ fromEnum t)
        M.Mu t r' -> liftM (S.Mu (toEnum $ fromEnum t)) (loop r')
        _ M.:+: _ ->
          do n <- inc
             m <- gather r
             return $ S.Sigma n m
               where
                 gather (r1 M.:+: r2) =
                   do r1' <- gather r1
                      r2' <- gather r2
                      return $ Map.union (Map.mapKeys (L :) r1') (Map.mapKeys (R :) r2')
                 gather r = liftM (Map.singleton []) $ loop r

count :: (Ord a, Show a) => S.Reg a -> PVal a -> Map Label (Map Path Int)
count r v = loop Map.empty r v
  where
    loop e r v =
      case (r, v) of
        (r1 S.:*: r2, v1 `Pair` v2) -> merge (loop e r1 v1) (loop e r2 v2)
            where
              merge = Map.unionWith $ Map.unionWith (+)
        (S.Var t, _) -> loop e (e ! t) v
        (S.Mu t r', Fold v') -> loop (Map.insert t r e) r' v'
        -- Star isn't working!!!
        -- (Star' r', In vs) -> Map.unions $ fmap (loop e r') vs
        (S.Sigma t rs, _) ->
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

arrange :: (Ord a, Show a) => S.Reg a -> Map Label (Map Path Int) -> M.Reg a
arrange r m = loop (fmap (Set.fromList . fmap swap . Map.toList) m) r
  where
    loop :: (Ord a, Show a) => Map Label (Set (Int, Path)) -> S.Reg a -> M.Reg a
    loop m r =
      case r of
        S.O -> M.O
        S.E -> M.E
        S.Lit a -> M.Lit a
        r1 S.:*: r2 -> loop m r1 M.:*: loop m r2
        S.Var t -> M.Var (toEnum $ fromEnum t)
        S.Mu t r' -> M.Mu (toEnum $ fromEnum t) $ loop m r'
        S.Sigma t rs -> hoffman $ Set.map (second (rs' !)) $ m ! t
          where
            rs' = fmap (loop m) rs

hoffman :: (Ord a, Show a) => Set (Int, M.Reg a) -> M.Reg a
hoffman s =
  case Set.minView s' of
    Just ((n2, r2), s'') -> hoffman (Set.insert (n1 + n2, r1 M.:+: r2) s'')
    Nothing -> r1
  where
    ((n1, r1), s') = fromJust $ Set.minView s

balance :: M.Reg a -> M.Reg a
balance r =
  case r of
    M.O -> M.O
    M.E -> M.E
    M.Lit a -> M.Lit a
    r1 M.:*: r2 -> balance r1 M.:*: balance r2
    M.Var t -> M.Var t
    M.Mu t r -> M.Mu t (balance r)
    _ M.:+: _ -> tree . map balance $ gather r
      where
        gather (r1 M.:+: r2) = gather r1 ++ gather r2
        gather r = [r]
        tree [] = M.O
        tree [r] = r
        tree rs = tree rs1 M.:+: tree rs2
          where
            (rs1, rs2) = splitAt (length rs `div` 2) rs

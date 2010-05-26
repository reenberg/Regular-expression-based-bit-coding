module Parse
(
    match,
    parse,
    flatten
)
where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (MonadPlus, forM, liftM, liftM2, mzero, mplus, msum)
import Control.Monad.State (State, get, put)
import Regex (Regex (..), STree (..))

match :: Eq a => Regex a -> [a] -> Bool
match O         _    = False
match E         []   = True
match (Lit c1)  [c2] = c1 == c2
match (e :+: f) cs   = match e cs || match f cs
match (e :*: f) cs   = or (matchPair `fmap` splits)
                       where
                         splits = [ splitAt n cs | n <- [0..length cs] ]
                         matchPair (cs1,cs2) = match e cs1 && match f cs2
match (S r)     cs   = null cs || or (map matchPair splits)
                       where
                         splits = [ splitAt n cs | n <- [0..length cs] ]
                         matchPair (cs1,cs2) = match r cs1 && match (S r) cs2
match _         _    = False

parse1 :: (MonadPlus m, Eq a) => Regex a -> [a] -> m (STree a)
parse1 O         _   = mzero
parse1 E         []  = return Unit
parse1 (Lit x)   [y] | x == y = return (Char x)
parse1 (e :+: f) cs  = liftM Inl (parse1 e cs) `mplus` liftM Inr (parse1 f cs)
parse1 (e :*: f) cs  = msum (parsePair `fmap` splits)
                       where
                         splits = [ splitAt n cs | n <- [0..length cs] ]
                         parsePair (cs1,cs2) = liftM2 Pair (parse1 e cs1) (parse1 f cs2)
parse1 (S e)     []  = return ((Fold . Inl) Unit)
parse1 (S e)     cs  = liftM (Fold . Inr) (msum (parsePair `fmap` splits))
                       where
                         splits = [ splitAt n cs | n <- [1..length cs] ]
                         parsePair (cs1,cs2) = liftM2 Pair (parse1 e cs1) (parse1 (S e) cs2)
parse1 _         _   = mzero

parse :: Eq a => Regex a -> [a] -> STree a
parse e cs = case parse1 e cs of Just v -> v

flatten :: STree a -> [a]
flatten Unit         = []
flatten (Char x)     = [x]
flatten (Inl v)      = flatten v
flatten (Inr w)      = flatten w
flatten (v `Pair` w) = flatten v ++ flatten w
flatten (Fold v)     = flatten v

--
--
--

data RegPath = Pstart
             | Pleft RegPath
             | Pright RegPath
             | Pfirst RegPath
             | Plast RegPath
             | Pin RegPath
     deriving (Eq, Ord, Show)

type St = Map (RegPath) (Set Int)

{- retrieve the state of the given path -}
getstate :: RegPath -> St -> Set Int
getstate path state = case Map.lookup path state of
                        Nothing -> Set.empty
                        Just s -> s

{- add the given position to the state of the given path -}
updstate :: Int -> RegPath  -> St -> St
updstate pos path state = Map.insert path (Set.insert pos (getstate path state)) state

{- Find the tail from position n in a list -}
nthtail :: Int -> [a] -> [a]
nthtail 0     xs     = xs
nthtail _     []     = []
nthtail (n+1) (x:xs) = nthtail n xs

{-
parsedyn1 :: (Ord a)
  => Int
  -> RegPath
  -> St
  -> Regex a
  -> [a]
  -> (Set (STree a, Int), St)
parsedyn1 pos path state re w =
  if Set.member pos (getstate path state)
  then (Set.empty, state)
  else let state1 = updstate pos path state in case re of
    O           -> (Set.empty, state1)
    E           -> (Set.singleton (Unit, pos), state1)
    (Lit a)     -> if null w || (not (head w == a))
                     then (Set.empty, state1)
                     else (Set.singleton (Char a, pos+1), state1)
    (r1 :+: r2) -> let (s1, state2) = parsedyn1 pos (Pleft path) state1 r1 w
                       (s2, state3) = parsedyn1 pos (Pright path) state2 r2 w
                   in (Set.union (Set.map (\(v,p) -> (Inl v,p)) s1) (Set.map (\(v,p) -> (Inr v,p)) s2), state3)
--    (r1 :*: r2) -> 
--    (S r)       -> 
-}

parsedyn1 :: (Ord a)
  => Int
  -> RegPath
  -> Regex a
  -> [a]
  -> State St (Set (STree a, Int))
parsedyn1 pos path re w =
  do state <- get
     if Set.member pos (getstate path state)
      then return Set.empty
      else let state1 = updstate pos path state in
       do put state1
          case re of
            O           -> return Set.empty
            E           -> return (Set.singleton (Unit, pos))
            (Lit a)     -> if null w || (not (head w == a))
                             then return Set.empty
                             else return (Set.singleton (Char a, pos+1))
            (r1 :+: r2) -> do s1 <- parsedyn1 pos (Pleft path) r1 w
                              s2 <- parsedyn1 pos (Pright path) r2 w
                              return (Set.union (Set.map (\(v,p) -> (Inl v,p)) s1) (Set.map (\(v,p) -> (Inr v,p)) s2))
--     (r1 :*: r2) -> 
--     (S r)       -> 

{-
parsedyn :: (Ord a) => Regex a -> [a] -> STree a
parsedyn e cs = w
  where
    (w):_ = [ v | (v,n) <- Set.toList q, n == length cs - 1 ]
    (q,_) = parsedyn1 0 Pstart Map.empty e cs
-}

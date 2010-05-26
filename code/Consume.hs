module Consume
(
    Consume,
    pop,
    evalConsume
)
where

import Control.Monad.State

type Consume c a = State [c] a

pop :: Consume c c
pop = do (c:cs) <- get
         put cs
         return c

evalConsume :: [c] -> Consume c a -> a
evalConsume cs st = evalState st cs

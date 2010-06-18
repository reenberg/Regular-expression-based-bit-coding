module Main where

import Prelude hiding (sum)
import RegKleene (Reg (..), PVal (..), flatten, alphanum, prod, sum, string, cclass, dot)
import qualified RegMu as M
import Parse (parse)
import Code (code, decode)
import Optimize (specialize, optimize, normalize, balance)
import Char (chr)

parse' r cs = case parse r cs of Just v -> v

title :: Reg Char
title = Star (alphanum :+: cclass " ,./():-&#;'?`@!+*=_[]|")

key :: Reg Char
key = Star (Star (cclass ['a' .. 'z']) :*: Lit '/') :*: Star (cclass $ "-" ++ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'])

tag :: Reg Char
tag = string "<tag key=\"" :*: key :*: string "\">" :*: title :*: string "</tag>\n"

tags :: Reg Char
tags = string "<?xml version=\"1.0\"?>\n<dblptags>\n" :*: Star tag :*: string "</dblptags>\n"
-- tags = Star dot

tags' = optimize tags

main =
  do cs <- readFile "../data/tags_small.xml"
     print tags'
     let v = parse' tags' cs
         tags'' = specialize tags' v
         v' = parse' tags'' cs
     print tags''
     let bs = code tags'' v'
     print bs
     print $ length bs

--
--
--

r1 = Star (Lit 'a' :+: Lit 'b' :+: Lit 'c')
r2 = Star (Lit 'd' :*: r1)
r3 = Star (r1 :*: r2)

txt = "dbdb"

v = parse' (normalize r3) txt

r4 = specialize (normalize r3) v

main_ =
  do print $ v
     print $ r4
     let stree = parse' r4 txt
     -- print $ stree
     print $ M.flatten stree
     print $ code r4 v
     print $ code r4 stree
     -- print $ (flatten . decode regex . code regex) stree

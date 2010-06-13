module Main where

import Prelude hiding (sum)
import Regex (Regex (..), STree (..), Var, flatten, alphanum, prod, sum, string, cclass, dot)
import Parse (parse)
import Code (code, decode)
import Optimize (optimize, balance)
import Char (chr)

title :: Regex Char
title = Star (alphanum :+: cclass " ,./():-&#;'?`@!+*=_[]|")

key :: Regex Char
key = Star (Star (cclass ['a' .. 'z']) :*: Lit '/') :*: Star (cclass $ "-" ++ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'])

tag :: Regex Char
tag = string "<tag key=\"" :*: key :*: string "\">" :*: title :*: string "</tag>\n"

tags :: Regex Char
--tags = string "<?xml version=\"1.0\"?>\n<dblptags>\n" :*: Star tag :*: string "</dblptags>"
tags = Star dot

main =
  do cs <- readFile "tags.xml"
     print (optimize tags)
     let v = case parse (optimize tags) cs of Just w -> w
     print v
     let bs = code (optimize tags) v
     print $ length bs

--
--
--

r1 = Star (Lit 'a' :+: Lit 'b' :+: Lit 'c')
r2 = Star (Lit 'd' :*: r1)
r3 = Star (r1 :*: r2)

parse' r cs = case parse r cs of Just v -> v

main_ =
  do print $ optimize r3
     let stree = parse' (optimize r3) "adadabcc"
     print $ stree
     print $ flatten stree
     print $ code (optimize r3) stree
     --print $ (flatten . decode regex . code regex) stree

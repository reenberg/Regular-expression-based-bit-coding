module Main where

import Prelude hiding (sum)
import RegKleene (Reg (..), PVal (..), flatten, alphanum, prod, sum, string, cclass, dot)
import qualified RegMu as M
import Parse (parse)
import Code (code, decode)
import Optimize (specialize, optimize, normalize, balance)
import Char (chr)
import qualified DTDParser.DTDParser as DTDParser (parse)
import qualified Data.Maybe as Maybe

parse' r s = Maybe.fromJust $ parse r s

title :: Reg Char
title = Star (alphanum :+: cclass " ,./():-&#;'?`@!+*=_[]|")

<<<<<<< HEAD:code/Main.hs
key :: Regex Char
key = Star (Star alphanum :*: Lit '/') :*: Star (alphanum :+: Lit '-')
=======
key :: Reg Char
key = Star (Star (cclass ['a' .. 'z']) :*: Lit '/') :*: Star (cclass $ "-" ++ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'])
>>>>>>> fb72635307f21cfcbe85d1c61723d76ce8945155:code/Main.hs

tag :: Reg Char
tag = string "<tag key=\"" :*: key :*: string "\">" :*: title :*: string "</tag>\n"

tags :: Reg Char
tags = string "<?xml version=\"1.0\"?>\n<dblptags>\n" :*: Star tag :*: string "</dblptags>\n"
-- tags = Star dot

tags' = optimize tags

main =
    do regex <- DTDParser.parse "../data/dblp.dtd"
       let regex' = optimize regex
       -- print regex
       cs <- readFile "../data/dblp_small.xml"
       let v = parse' regex' cs
           regex'' = specialize regex' v
           v' = parse' regex'' cs
       -- print regex'
       let bs = code regex'' v'
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

module Main where

import Prelude hiding (sum)
import Regex (Regex (..), STree (..), Var, flatten, alphanum, prod, sum, string, cclass, dot)
import Parse (parse)
import Code (code, decode)
import Optimize (specialize, optimize, normalize, balance)
import Char (chr)
import qualified DTDParser.DTDParser as DTDParser (parse)
import qualified Data.Maybe as Maybe

parse' r s = Maybe.fromJust $ parse r s

title :: Regex Char
title = Star (alphanum :+: cclass " ,./():-&#;'?`@!+*=_[]|")

key :: Regex Char
key = Star (Star alphanum :*: Lit '/') :*: Star (alphanum :+: Lit '-')

tag :: Regex Char
tag = string "<tag key=\"" :*: key :*: string "\">" :*: title :*: string "</tag>\n"

tags :: Regex Char
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
r2 = normalize $ Star (Lit 'd' :*: r1)
r3 = Star (r1 :*: r2)

txt = "dbdb"

v = parse' r2 txt

r4 = specialize r2 v

main_ =
  do print $ r2
     print $ v
     print $ r4
     let stree = parse' r4 txt
     -- print $ stree
     print $ flatten stree
     print $ code r2 v
     print $ code r4 stree
     -- print $ (flatten . decode regex . code regex) stree

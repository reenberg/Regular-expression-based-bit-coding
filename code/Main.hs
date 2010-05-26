--
--
--

module Main
(
)
where

import Regex (Regex (..), STree (..))
import Coding (code, decode)
import Parse (match, parsedynamic, flatten)

string :: String
string = "abcbcba"

regex1 :: Regex Char
regex1 = S ((Lit 'a' :+: Lit 'b') :+: Lit 'c')

regex2 :: Regex Char
regex2 = Lit 'a' :*: S (Lit 'b' :+: Lit 'c') :*: Lit 'a'

parse :: Ord a => Regex a -> [a] -> STree a
parse e cs = case parsedynamic e cs of Just v -> v

main :: IO ()
main =
    do putStrLn "regex 1:"
       putStrLn ""
       print $ match regex1 string
       print $ regex1
       print $ parse regex1 string
       print $ (code regex1 . parse regex1) string
       print $ (length . code regex1 . parse regex1) string
       print $ (flatten . decode regex1 . code regex1 . parse regex1) string
       putStrLn ""
       putStrLn "regex 2:"
       putStrLn ""
       print $ match regex2 string
       print $ regex2
       print $ parse regex2 string
       print $ (code regex2 . parse regex2) string
       print $ (length . code regex2 . parse regex2) string
       print $ (flatten . decode regex2 . code regex2 . parse regex2) string

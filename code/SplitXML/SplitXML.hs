-- module SplitXML.SplitXML
module Main
where

import Text.XML.HXT.Arrow
import qualified Data.Tree.NTree.TypeDefs as NTree
import qualified Text.XML.HXT.DOM.TypeDefs as XNode
import qualified Text.XML.HXT.DOM.XmlNode as XN

import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit

import Data.Maybe


main :: IO ()
main = do
  argv <- getArgs -- argv!!0 should contain xml file name
  if length argv /= 1
     then do print "You need to supply the filename as first argument, and nothing more."
             exitWith (ExitFailure (-1))
     else do let conf = [(a_validate, "0")]
             xml <- runX (readDocument conf (argv!!0))
             let xml' = stripMarkup $ head xml
             print xml'
             exitWith ExitSuccess


stripMarkup (NTree.NTree n ts) =
    NTree.NTree
             (case n of
                XNode.XText s -> XNode.XText "foo"
                XNode.XTag n ts -> XNode.XTag n $ fmap stripMarkup ts
                _ -> n) $
             fmap stripMarkup ts

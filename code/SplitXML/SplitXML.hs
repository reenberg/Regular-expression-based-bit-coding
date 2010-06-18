module SplitXML.SplitXML

import Text.XML.HXT.Arrow
import qualified Data.Tree.NTree.TypeDefs as NTree
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
             return $ exitWith (ExitFailure (-1))
     else do let conf = [(a_validate, "0")]
             newXml <- runX (getMarkupOnly conf (argv!!0))                              
             print newXml
             return $ exitWith ExitSuccess



getMarkupOnly conf fileName = 
    readDocument conf fileName >>>
    processTopDown stripMarkup >>>
    getChildren


stripMarkup
